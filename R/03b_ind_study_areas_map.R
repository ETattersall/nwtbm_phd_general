###############################
## 03b_ind_study_areas_map.R
## Generating maps for individual study areas containing
## Created by Erin Tattersall
## January 22 2026
## If objects are missing, check 01_merge_station_locations.R
###########################


#### Environment set up ####
## Load required packages (should already be installed)

x <- c("sf",
       "osmdata", 
       "stars",
       "spatialEco",
       "MODISTools",
       "ggmap",
       "raster",
       "ggspatial",
       "cowplot",
       "leaflet",
       "terra", 
       "maptiles", 
       "ggplot2", 
       "tidyterra", 
       "ggspatial",
       "wildrtrax",
       "tidyverse",
       "data.table",
       "patchwork")



## install.packages(x) ## should already be installed ####
lapply(x, require, character.only = TRUE)

## set working directory to file with NWT shapefiles
getwd()
list.files("data/NWT_GIS_Data")


#### Sambaa K'e map
## Load in NWT boundary and Sambaa K'e shapefiles
nwt.boun <- vect("data/NWT_GIS_Data/NWT_boundary.shp")
sk.boun <- vect("data/NWT_GIS_Data/Sambaa_Ke_protected_area.shp")


crs(nwt.boun, proj = TRUE)
crs(sk.boun, proj = TRUE) ## both are NAD 83 - will need to be transformed to WGS 84 for leaflet


## Converting to sf objects in WGS 84 for leaflet
nwt_sf <- nwt.boun %>% st_as_sf() %>% 
          st_transform(crs = 4326)
sk_sf <- sk.boun %>% st_as_sf() %>% 
         st_transform(crs = 4326)

crs(nwt_sf)
st_bbox(nwt_sf)
crs(sk_sf)
st_bbox(sk_sf)

## Create sf point object for Sambaa K'e community, coordinates at: 60.44250 N, -121.24528 W
sk_point <- st_as_sf(data.frame(name = "Sambaa K'e",
                                   lat = 60.44250,
                                   lon = -121.24528),
                         coords = c("lon", "lat"),
                         crs = 4326)

### Map with ggplot2 and ggspatial 


# Get ESRI basemap (e.g., "World_Imagery" or "World_Topo_Map")
basemap <- get_tiles(nwt_sf, provider = "Esri.WorldImagery", crop = TRUE, zoom = 8) # NWT boundary polygon used to get extent of basemap, zoom level can be adjusted
# note: higher resolution base imagery takes longer to download and display

win.graph() # open separate graphics window
gg_map <- ggplot() +
  layer_spatial(basemap) + # add basemap
  geom_sf(data = sk_sf, fill = NA, linewidth = 2, color = "black") + # Sambaa K'e candidate protected area boundary
  geom_sf(data = sk_point, color = "yellow3", size = 3, show.legend = FALSE) + # Sambaa K'e community point
  # Add the label slightly offset to the right and a touch up
  geom_sf_text(
    data = sk_point,
    aes(label = "Sambaa K’e"),
    nudge_x = 0.30,     # degrees of longitude (adjust as needed)
    nudge_y = 0.05,     # degrees of latitude (adjust as needed)
    size = 4.5,
    color = "white") +
  labs(x = "Longitude",
       y = "Latitude") +
  scale_y_continuous(limits = c(60,61.5)) + # set latitude range
  scale_x_continuous(limits = c(-123,-119)) + # set longitude range
  theme_classic()

gg_map

### Add NWT inset with sk_sf bounding box -- see last co-pilot conversation and test that code

