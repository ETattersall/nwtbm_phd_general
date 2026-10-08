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


### Inspect fire severity data before loading - these data only cover between 1985 - 2022
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
plot(fsev_sa_20km) ## values range from 0 - 1.52. Includes all areas within SA polygons (including water) as 0

#### Mask water bodies so only unburned land = 0, not water
## read in water polygons cropped to study areas (done in script 07b)
sa_water <- vect("data/LCC_2020/nwtbm_study_areas_water_polygons.gpkg")
sa_water

# re-project to match raster projection
sa_water <- project(sa_water, fsev_sa_20km)

glimpse(sa_water)
plot(sa_water)

## Mask water out of fsev_sa_20km
fsev_sa_20km <- mask(x = fsev_sa_20km,
                     mask = sa_water,
                     inverse = TRUE) # removes water polygons, keeps everything else within the study areas

plot(fsev_sa_20km)
summary(fsev_sa_20km)
hist(fsev_sa_20km$`CA_Forest_Wildfire_dNBR_1985-2022`)

## save study area fire severity data as raster
writeRaster(fsev_sa_20km, "data/CA_Forest_Wildfire_dNBR_1985-2022/NWT_studyareas_20km_wildfire_dNBR_1985-2022.tif", overwrite = TRUE)

## Read in raster
# fsev_sa_20km <- rast("data/CA_Forest_Wildfire_dNBR_1985-2022/NWT_studyareas_20km_wildfire_dNBR_1985-2022.tif")


# ### Map fire severity in study areas - not yet updated with water masked
# gg_sa_fsev <- ggplot() +
#   geom_spatraster(data= fsev_sa_20km, use_coltab = TRUE) +
#   geom_sf(data = sa_sf, fill = NA, color = "black", linewidth = 1) +
#   scale_fill_gradient(low = "white", high = "red",na.value = "transparent") +  # Make NA values blank
#   coord_sf() +
#   labs(title = "Fire Severity in NWTBM Study Areas", 
#        x = "Longitude",
#        y = "Latitude",
#        fill = "Fire Severity (dNBR)") +
#   theme_classic() +
#     # increase size of title text, axis text, and facet titles
#     theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
#     theme(axis.title.x = element_text(size = 16)) +
#     theme(axis.title.y = element_text(size = 16)) +
#     theme(axis.text = element_text(size = 12)) +
#     theme(legend.title= element_text(size = 16))
# 
# win.graph()
# gg_sa_fsev
# 
# ## Save plot
# ggsave("figures/fire_explore/ntems_fireseverity_1985-2022_studyareas.jpeg", gg_sa_fsev, width = 12, height = 8, dpi = 300)


### Extract fsev values around stations and site buffers
glimpse(stations_500m)
crs(stations_500m)
class(stations_500m)


fsev_stns_mean <- extract(fsev_sa_20km, stations_500m, 
                          fun = mean, # calculating the mean of all cells within the station buffer
                          na.rm = TRUE)[[2]] ## removing NA values (na.rm) and only selecting the column with the mean values ([[2]])
class(fsev_stns_mean) #numeric

summary(fsev_stns_mean) # values range from 0 - 0.995

fsev_site_mean <- extract(fsev_sa_20km, sites_500m, 
                          fun = mean,
                          na.rm = TRUE)[[2]]
summary(fsev_site_mean) # values range from 0 - 0.84

hist(fsev_stns_mean)
hist(fsev_site_mean)

## Compare these mean extracted values to max extracted values
fsev_stns_max <- extract(fsev_sa_20km, stations_500m, 
                         fun = max,
                         na.rm = TRUE)[[2]]
summary(fsev_stns_max) ## between 0 - 1.58
hist(fsev_stns_max)

fsev_site_max <- extract(fsev_sa_20km, sites_500m, 
                         fun = max,
                         na.rm = TRUE)[[2]]
summary(fsev_site_max) ## between 0 - 1.58 as well (makes sense that the highest max value would be the same)
hist(fsev_site_max)

## So averaging fire severity dilutes it quite a bit...

## These methods also don't account for fires of different ages being within a single buffer. 
## To do that, I want to apply the same rule as with dealing with ages of multiple fires: calculate the mean/max of the most recent fire if it's >= 10% of the buffered area

## First, load fire polygon data and rasterize it
fire_poly <- vect("data/nrcan_nbac/NBAC_1972to2024_20250506_shp/NBAC_fires_by_study_area_20kmbuffer.shp")

# re-project to match raster projection
fire_poly <- project(fire_poly, fsev_sa_20km)

glimpse(fire_poly) # need column YEAR

names(fire_poly)
head(fire_poly$YEAR)
class(fire_poly$YEAR)

fire_year <- rasterize(fire_poly,
                       fsev_sa_20km,
                       field = "YEAR",
                       touches = TRUE)

fire_year
summary(fire_year)

## Combine with fsev
fire_stack <- c(fsev_sa_20km, fire_year)
names(fire_stack) <- c("severity", "fire_year")

## Extract fire severity and year data for stations and sites
fire_vals_stns <- extract(fire_stack, stations_500m, 
                          cells = TRUE)
summary(fire_vals_stns) #ID = 1 - 822, corresponding to stations

## Remove non-fire cells
fire_vals_stns2 <- fire_vals_stns |> filter(!is.na(fire_year))

## Calculate area represented by one raster cell
cell_area <- prod(res(fsev_sa_20km))

## Count cells by station and fire year
year_area <- fire_vals_stns2 %>%
  group_by(ID, fire_year) %>%
  summarise(
    n_cells = n(),
    mean_severity = mean(severity, na.rm = TRUE),
    .groups = "drop"
  )

## Add buffer, compute proportions
buffer_area <- terra::expanse(stations_500m, unit = "m")

year_area <- year_area %>%
  mutate(
    burned_area = n_cells * cell_area,
    buffer_area = buffer_area[ID],
    proportion_burned = burned_area / buffer_area
  )

## Applying fire selection rule
selected_fire_stns <- year_area %>%
  arrange(ID, desc(fire_year)) %>%
  group_by(ID) %>%
  slice(
    {
      idx <- which(proportion_burned >= 0.10)
      
      if (length(idx) > 0) idx[1] else 1
    }
  ) %>%
  ungroup()

summary(selected_fire_stns)

## join fire metrics to station metadata

# add ID column to stations_500m
stations_500m$ID <- seq_len(nrow(stations_500m))
summary(stations_500m)

final_fire_stns <- stations_500m %>%
  st_drop_geometry() %>%
  select(ID, study_area, site, location) %>%
  left_join(
    selected_fire_stns,
    by = "ID"
  ) %>%
  mutate(
    mean_severity = replace_na(mean_severity, 0),
    burned_area = replace_na(burned_area, 0),
    proportion_burned = replace_na(proportion_burned, 0)
  )

glimpse(final_fire_stns)
summary(final_fire_stns)

## Calculate the same for sites
fire_vals_sites <- extract(fire_stack, sites_500m, 
                          cells = TRUE)

summary(fire_vals_sites) #ID = 1 - 231, corresponding to sites

## Remove non-fire cells
fire_vals_sites2 <- fire_vals_sites |> filter(!is.na(fire_year))


## Count cells by sites and fire year
year_area_sites <- fire_vals_sites2 %>%
  group_by(ID, fire_year) %>%
  summarise(
    n_cells = n(),
    mean_severity = mean(severity, na.rm = TRUE),
    .groups = "drop"
  )

## Add buffer, compute proportions
buffer_area_sites <- terra::expanse(sites_500m, unit = "m")

year_area_sites <- year_area_sites %>%
  mutate(
    burned_area = n_cells * cell_area,
    buffer_area = buffer_area_sites[ID],
    proportion_burned = burned_area / buffer_area
  )

## Applying fire selection rule
selected_fire_sites <- year_area_sites %>%
  arrange(ID, desc(fire_year)) %>%
  group_by(ID) %>%
  slice(
    {
      idx <- which(proportion_burned >= 0.10)
      
      if (length(idx) > 0) idx[1] else 1
    }
  ) %>%
  ungroup()

summary(selected_fire_sites)

## join fire metrics to station metadata

# add ID column to stations_500m
sites_500m$ID <- seq_len(nrow(sites_500m))
summary(sites_500m)

final_fire_sites <- sites_500m %>%
  st_drop_geometry() %>%
  select(ID, study_area, site) %>%
  left_join(
    selected_fire_sites,
    by = "ID"
  ) %>%
  mutate(
    mean_severity = replace_na(mean_severity, 0),
    burned_area = replace_na(burned_area, 0),
    proportion_burned = replace_na(proportion_burned, 0)
  )

glimpse(final_fire_sites)
summary(final_fire_sites)

## Convert to data frames add raw mean and max scores to final_fire dfs
final_fire_stns <- data.frame(final_fire_stns)
final_fire_sites <- data.frame(final_fire_sites)

## add fsev_stns_mean and fsev_stns_max to final_fire_stns
final_fire_stns$fsev_mean_raw <- fsev_stns_mean
final_fire_stns$fsev_max_raw <- fsev_stns_max
summary(final_fire_stns) ## means are very similar

## add fsev_site_mean and fsev_site_max to final_fire_sites
final_fire_sites$fsev_mean_raw <- fsev_site_mean
final_fire_sites$fsev_max_raw <- fsev_site_max
summary(final_fire_sites) ## means are very similar here too

write.csv(final_fire_stns, "data/nwtbm_sensor_ntems_fireseverity.csv")
write.csv(final_fire_sites, "data/nwtbm_sites_ntems_fireseverity.csv")


#### UN SPIDER burn severity data ####
## Note: Likely will not use UN SPIDER burn severity data for more recent years. 
## Rationale - the dNBR values do not compare well with NTEMS severity metrics, 
## and there are only 13 stations/5 sites for which 2023 (i.e., recent years NTEMS doesn't cover) fire severity data would be needed
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
summary(dNBR2022) ## values between -1088 - 1537: will need to be converted


# Visual comparison to fsev_sa_20km (in separate window)
win.graph()
plot(fsev_sa_20km) ## values between 0 - 1.6.



glimpse(fire_poly)

## filter fire_poly for 2022 fires only, for both fsev_sa_20km and dNBR2022
fire_poly2022 <- fire_poly |> filter(YEAR == 2022)
fire_poly2022
plot(fire_poly2022)

## Mask both dNBR2022 and fsev_sa_20km by fire_poly2022

dNBR2022_mask <- mask(x = dNBR2022,
                      mask = fire_poly2022)
win.graph()
plot(dNBR2022_mask)
summary(dNBR2022_mask) ## over 100 000 NAs

fsev2022_mask <- mask(x = fsev_sa_20km,
                      mask = fire_poly2022)
win.graph()
plot(fsev2022_mask)
summary(fsev2022_mask) ## also has over 100 000 NAs

## Check minimum and maximum values to compare scales
minmax(dNBR2022_mask)
minmax(fsev2022_mask)



global(dNBR2022_mask, range, na.rm = TRUE)
global(fsev2022_mask, range, na.rm = TRUE)
hist(values(dNBR2022_mask))
win.graph()
hist(values(fsev2022_mask))

## dNBR2022 is normally distributed around 0, fsev is more zero-inflated. Likely that the SPIDER data includes negative spectral values that are all just considered unburned (0) by NTEMS?

## Extract around sites (don't summarize, remove NAs)
spider2022_sites <- extract(dNBR2022_mask,
                            sites_500m,
                            cells = TRUE,
                            na.rm = TRUE)

ntems2022_sites <- extract(fsev2022_mask,
                            sites_500m,
                            cells = TRUE,
                            na.rm = TRUE)
summary(spider2022_sites)
summary(ntems2022_sites)

## Combine as df
sev_compare2022 <- cbind(ntems2022_sites, spider2022_sites$nd)

summary(sev_compare2022) #spider data has 23 NAs (~2/3s of the polygons), ntems has none

## Might be challenging to aggregate fire severity data from other sources... How many 2023-2024 fires would fall within site and station buffers anyway?
## Only the 2023 Fort Smith fires affected any sites (5) and stations (13)
## Okay with these being NA? Or should we derive proxy values based on SPIDER (can we?)


## (trying to assess 2023 and 2024 fire polygons at my sites - unnecessary)
# fires_23_24 <- fire_poly |> filter(YEAR == 2023 | YEAR == 2024) |> st_as_sf()
# fires_23_24
# plot(fires_23_24)
# glimpse(fires_23_24) # 68 fires in study areas in 2023-2024
# 
# ## Which study areas did these fires occur in?
# unique(fires_23_24$study_area) ## all of them - but only Fort Smith, Gameti, and Norman Wells had deployments out during those fire seasons
# 
# fires_23_24_deps <- fires_23_24 |> filter(study_area == "FortSmith" |
#                                             study_area == "NormanWells" |
#                                             study_area == "Gameti")
# #
# 
# 
# # re-project sites_500m to match fire_poly
# sites_500m <- project(sites_500m, fires_23_24)
# 
# fires_23_24_sites <- crop(fires_23_24, sites_500m)
# glimpse(fires_23_24_sites)
# 
# ## Troubleshooting Topology exception error
# geomtype(fires_23_24) #polygons
# geomtype(sites_500m) #polygons
# 
# nrow(fires_23_24) #68
# nrow(sites_500m) #231
# 
# same.crs(fires_23_24, sites_500m) #TRUE
# 
# ## re-buffering sites_500m in case topology issue was introduced during buffering
# 
# sites_500m <- vect(
#   st_make_valid(st_as_sf(sites_500m))
# ) # still didn't work
# 
# ## Try dissolving overlapping buffers
# sites_dissolved <- aggregate(sites_500m)
# glimpse(sites_dissolved)
# plot(sites_dissolved)
