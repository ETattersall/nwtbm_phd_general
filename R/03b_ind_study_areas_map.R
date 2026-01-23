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
crs(sk.boun, proj = TRUE) ## both are NAD 83


## Converting to sf objects for leaflet
nwt_sf <- nwt.boun %>% st_as_sf() %>% 
          st_transform(crs = 3573) # in Canada Atlas Lambert (for wider Canada projection)
sk_sf <- sk.boun %>% st_as_sf() %>% 
         st_transform(crs = 4326) # in WGS 84

crs(nwt_sf, proj = TRUE)
st_bbox(nwt_sf)
crs(sk_sf)
st_bbox(sk_sf)

## Add a buffer of a degree of longitude and 0.5 degree of latitude around Sambaa K'e for basemap extent
sk_sf_buffer <- st_as_sfc(st_bbox(sk_sf) + c(-1, -0.5, 1, 0.5)) %>%
  st_set_crs(st_crs(sk_sf))

## Create sf point object for Sambaa K'e community, coordinates at: 60.44250 N, -121.24528 W
sk_point <- st_as_sf(data.frame(name = "Sambaa K'e",
                                   lat = 60.44250,
                                   lon = -121.24528),
                         coords = c("lon", "lat"),
                         crs = 4326)

### Map with ggplot2 and ggspatial 


# Get ESRI basemap (e.g., "World_Imagery" or "World_Topo_Map")
basemap <- get_tiles(sk_sf_buffer, provider = "Esri.WorldImagery", crop = TRUE, zoom = 8) 
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
    size = 5,
    color = "white") +
  labs(x = "Longitude",
       y = "Latitude") +
  # increase label sizes for axes titles and text
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  ) +
  scale_y_continuous(limits = c(60,61.5)) + # set latitude range
  scale_x_continuous(limits = c(-123,-119)) + # set longitude range
  theme_classic()

gg_map

#### Add NWT inset with sk_sf bounding box

## Create bounding box for Sambaa K'e
sk_bbox <- st_as_sfc(st_bbox(sk_sf)) %>%
  st_set_crs(st_crs(sk_sf)) %>%
  st_transform(crs = st_crs(nwt_sf)) # transform to NWT crs (Canada Atlas Lambert) for plotting

crs(sk_bbox, proj = TRUE)

## Inset map of NWT with Sambaa K'e bounding box
gg_inset <- ggplot() +
  geom_sf(data = nwt_sf, fill = "lightgreen", color = "black") + # NWT boundary
  geom_sf(data = sk_bbox, fill = NA, color = "darkred", linewidth = 1) + # Sambaa K'e bounding box
  theme_void() +
  theme(panel.background = element_rect(fill = "white"))

gg_inset



### Combine main map and inset using cowplot
## Really just playing with the left, bottom, right, top numbers until I have a position I'm happy with (function info says these are in npc units?)

sk_map <- 
  gg_map +
  inset_element(
    gg_inset,
    left   = 0.8,  # presumably left edge of inset?
    bottom = 0,  # 
    right  = 1,  # flush with right side of plot
    top    = 1.61,  # moving vertically (flush with top of plot - incremental adjustments make big diff. in latitude!)
    align_to = "panel")

sk_map

##Save map
ggsave("figures/SambaaKemap_PA_NWTinset_20260123.png", plot = sk_map, width = 10, height = 8, dpi = 600)
