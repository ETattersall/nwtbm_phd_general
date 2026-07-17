########################################
## 06_fire_severity_ntems.R
## Exploring NTEMS fire severity data
## Started on March 23 2026
## Created by Erin Tattersall
########################################

#### Environment set up ####
## Load required packages (should already be installed)

list.of.packages <- c("wildrtrax",
                      "sf",
                      "lwgeom",
                      "data.table",
                      "tidyverse",
                      "dplyr",
                      "osmdata", 
                      "stars",
                      "ggspatial",
                      "cowplot",
                      "leaflet",
                      "terra", 
                      "maptiles", 
                      "ggplot2", 
                      "tidyterra", 
                      "ggspatial",
                      "viridis",
                      "corrplot",
                      "kableExtra",
                      "lubridate",
                      "purrr")



# A check to see which ones I have and which are missing
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Code which tells R to install the missing packages
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


### Load study area polygons with and without 20km buffer
sa_20km <- vect("data/study_area_spatial/NWTBM_all_study_areas_20km_buffers.shp")
class(sa_20km) #SpatVector
sa_20km
sa_sf <- vect("data/study_area_spatial/NWTBM_all_study_areas.shp")
plot(sa_20km)

## Read in station and sites with 500m buffers from R/03_location_data.R
stations_500m <- vect("data/sensor_locations/nwtbm_sensor_locations_500mbuffer.gpkg")
sites_500m <- vect("data/sensor_locations/nwtbm_sensor_sites_500mbuffer.gpkg")


### Inspect fire severity data before loading - these data only cover up to 2022
## Data sourced from NTEMS: https://opendata.nfis.org/mapserver/nfis-change_eng.html
list.files("data/CA_Forest_Wildfire_dNBR_1985-2022")

describe("data/CA_Forest_Wildfire_dNBR_1985-2022/CA_Forest_Wildfire_dNBR_1985-2022.tif")

## load the fire severity raster file
fsev <- rast("data/CA_Forest_Wildfire_dNBR_1985-2022/CA_Forest_Wildfire_dNBR_1985-2022.tif")
fsev #SpatRaster class, projection Lambert_Conformal_Conic_2SP
crs(fsev)

win.graph()
plot(fsev)

summary(fsev) ## values range from 0 - 1.65

### dNBR is on a scale of 0 - 2, with higher dNBR values relating to higher burn severity

# ## Transform sa_20km to same projection as fsev
sa_20km <- sa_20km %>%
  project(fsev)
sa_20km # SpatVector of projection Lambert_Conformal_Conic_2SP

## Crop fsev to 20km SA buffers
fsev_sa_20km <- fsev %>% 
  crop(sa_20km, mask = TRUE) # mask = TRUE returns NA values for pixels outside sa_20km extent (otherwise crop just returns a rectangle)

summary(fsev_sa_20km)
plot(fsev_sa_20km) ## values range from 0 - 1.52


## save study area fire severity data as raster
writeRaster(fsev_sa_20km, "data/CA_Forest_Wildfire_dNBR_1985-2022/NWT_studyareas_20km_wildfire_dNBR_1985-2022.tif", overwrite = TRUE)

## Read in raster
# fsev_sa_20km <- rast("data/CA_Forest_Wildfire_dNBR_1985-2022/NWT_studyareas_20km_wildfire_dNBR_1985-2022.tif")


### Map fire severity in study areas
gg_sa_fsev <- ggplot() +
  geom_spatraster(data= fsev_sa_20km, use_coltab = TRUE) +
  geom_sf(data = sa_sf, fill = NA, color = "black", linewidth = 1) +
  scale_fill_gradient(low = "white", high = "red",na.value = "transparent") +  # Make NA values blank
  coord_sf() +
  labs(title = "Fire Severity in NWTBM Study Areas", 
       x = "Longitude",
       y = "Latitude",
       fill = "Fire Severity (dNBR)") +
  theme_classic() +
    # increase size of title text, axis text, and facet titles
    theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
    theme(axis.title.x = element_text(size = 16)) +
    theme(axis.title.y = element_text(size = 16)) +
    theme(axis.text = element_text(size = 12)) +
    theme(legend.title= element_text(size = 16))

win.graph()
gg_sa_fsev

## Save plot
ggsave("figures/fire_explore/ntems_fireseverity_1985-2022_studyareas.jpeg", gg_sa_fsev, width = 12, height = 8, dpi = 300)


### Extract fsev values around stations and site buffers
glimpse(stations_500m)
crs(stations_500m)
class(stations_500m)


fsev_stns500 <- extract(fsev_sa_20km, stations_500m, fun = mean)[[2]]
class(fsev_stns500) #numeric

summary(fsev_stns500) # values range from 0 - 0.995

fsev_site500 <- extract(fsev_sa_20km, sites_500m, fun = mean)[[2]]
summary(fsev_site500) # values range from 0 - 0.76 


#### UN SPIDER burn severity data ####
## Calculated dNBR for more recent years using a UN Google Earth Engine: https://un-spider.org/advisory-support/recommended-practices/recommended-practice-burn-severity/burn-severity-earth-engine
## Used 20km buffers around all study areas, Landsat 8 imagery, and specified fire years (i.e., covering full fire season) for fire years not covered by above NTEMS data
## Temporal periods used for 2022 data: pre-fire imagery = 2022-05-01 to 2022-06-01, post-fire imagery = 2022-09-15 to 2022-10-15
## Load calculated burn severity for 2022

# list file names for 2022 dNBR data
tifs_2022 <-
  list.files(
    path = "data/un-spider_dNBR",
    pattern = "^UN-SPIDER_dNBR_2022.*\\.tif$",
    full.names = TRUE
  )

## Check whether output tifs are tiles or layers
for(f in tifs_2022) {
  r <- rast(f)
  cat(basename(f), "\n")
  print(r)
} # different spatial extents, 1 layer - these are raster tiles that need to be merged

## Load tifs as rasters
tiles_2022 <- lapply(tifs_2022, rast) 

## Merge raster tiles
dNBR2022 <- do.call(terra::merge, tiles_2022)
dNBR2022 ##EPSG:4326

## Match projection of fsev_sa_20km
dNBR2022 <- project(dNBR2022, fsev_sa_20km)
dNBR2022

win.graph()
plot(dNBR2022)
summary(dNBR2022) ## values between -1544 - 1219: will need to be converted



# Visual comparison to fsev_sa_20km (in separate window)
win.graph()
plot(fsev_sa_20km) ## values between 0 - 1.6


### Read in NBAC fire polygons to mask dNBR only to relevant areas (read in as SpatVector)
fire_poly <- vect("data/nrcan_nbac/NBAC_1972to2024_20250506_shp/NBAC_fires_by_study_area_20kmbuffer.shp")

# re-project to match raster projection
fire_poly <- project(fire_poly, fsev_sa_20km)

glimpse(fire_poly)

## filter for 2022 fires only, for both fsev_sa_20km and dNBR2022
fire_poly2022 <- fire_poly |> filter(YEAR == 2022)
crs(fire_poly2022)
plot(fire_poly2022)

## Mask both dNBR2022 and fsev_sa_20km by fire_poly2022

dNBR2022_mask <- mask(x = dNBR2022,
                      mask = fire_poly2022)
win.graph()
plot(dNBR2022_mask)
summary(dNBR2022_mask)

fsev2022_mask <- mask(x = fsev_sa_20km,
                      mask = fire_poly2022)
win.graph()
plot(fsev2022_mask)
summary(fsev2022_mask)

## Check minimum and maximum values to compare scales
minmax(dNBR2022_mask)
minmax(fsev2022_mask)



global(dNBR2022, range, na.rm = TRUE)
global(fsev2022_mask, range, na.rm = TRUE)
hist(values(dNBR2022))
hist(values(fsev2022_mask))

## Emailed James Maltman to ask about converting between these two scales (17 July 2026)
