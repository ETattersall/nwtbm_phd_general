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
       "ggrepel",
       "cowplot",
       "leaflet",
       "terra", 
       "maptiles",
       "mapedit",
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

## Load in NWT water bodies to include in map
nwt.water <- vect("data/NWT_GIS_Data/NWT_water.shp")
crs(nwt.water)

## Load in NWT roads (should include SK winter road)
nwt.roads <- vect("data/NWT_GIS_Data/NWT_roads.shp")
crs(nwt.roads)


## Converting to sf objects for leaflet, all in Canada Atlas Lambert projection
nwt_sf <- nwt.boun %>% st_as_sf() %>% 
          st_transform(crs = 3573) 
sk_sf <- sk.boun %>% st_as_sf() %>% 
         st_transform(crs = 3573)
water_sf <- nwt.water %>% st_as_sf() %>% 
            st_transform(crs = 3573)
roads_sf <- nwt.roads %>% st_as_sf() %>% 
            st_transform(crs = 3573)

crs(nwt_sf, proj = TRUE)
st_bbox(nwt_sf)
crs(sk_sf)
st_bbox(sk_sf)

## Add a buffer of a degree of longitude and 0.5 degree of latitude around Sambaa K'e for basemap extent
sk_sf_buffer <- st_as_sfc(st_bbox(sk_sf) + c(-1, -0.5, 1, 0.5)) %>%
  st_set_crs(st_crs(sk_sf))

## Crop water to sk_sf_buffer extent for plotting
sk_water <- st_intersection(water_sf, sk_sf_buffer)
plot(sk_water)

## Crop roads to sk_sf_buffer extent for plotting
sk_roads <- st_intersection(roads_sf, sk_sf_buffer)
plot(sk_roads) 
summary(sk_roads) ## more lines than expected (89 observations)... I only want the winter road. 2 features are named HWY, try filtering for those
sk_roads$TYPE ## only 2 features have names, and they are both HWY. Filter for those
sk_hwy <- sk_roads %>% filter(TYPE == "HWY")
plot(sk_hwy) # nope, this is Hwy 1 to Ft Simpson
class(sk_roads)

## use selectFeatures function from mapedit to interactively select the winter road feature
sk_winter_road <- sk_roads %>% 
  selectFeatures() # click on the winter road feature in the plot that opens (it's a line so cursor has to be precise)
plot(sk_winter_road)


## Create sf point object for Sambaa K'e community, coordinates at: 60.44250 N, -121.24528 W
sk_point <- st_as_sf(data.frame(name = "Sambaa K'e",
                                   lat = 60.44250,
                                   lon = -121.24528),
                         coords = c("lon", "lat"),
                         crs = 4326)

## Manually set position of Sambaa K'e label
sk_point_label <- st_transform(sk_point) %>%  
  st_geometry() %>% 
  `+`(c(0.15, -0.02))  # shift label slightly right and down

st_crs(sk_point_label, proj = TRUE) # check crs of label geometry
st_crs(sk_point_label) <- 4326

### Map with ggplot2 and ggspatial 


# NOT USING: Get ESRI basemap (e.g., "World_Imagery" or "World_Topo_Map")
## basemap <- get_tiles(sk_sf_buffer, provider = "Esri.WorldImagery", crop = TRUE, zoom = 8) 
# note: higher resolution base imagery takes longer to download and display

win.graph() # open separate graphics window
gg_map <- ggplot() +
  # layer_spatial(basemap) + # add basemap
  geom_sf(data = sk_water, fill = "blue3", color = "blue3") + # water bodies
  geom_sf(data = sk_winter_road, linewidth = 1, color = "gray50") + # roads
  geom_sf(data = sk_sf, fill = NA, linewidth = 2, color = "black") + # Sambaa K'e candidate protected area boundary
  geom_sf(data = sk_point, color = "red3", size = 3, show.legend = FALSE) + # Sambaa K'e community point
  # Add the label slightly offset to the right and a touch up
  geom_sf_text(
    data = st_set_geometry(sk_point, sk_point_label),
    aes(label = "Sambaa K'e"), 
    size = 5,
    color = "black") +
  labs(x = "Longitude",
       y = "Latitude") +
  # increase label sizes for axes titles and text
  theme(
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20)
  ) +
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
ggsave("figures/SambaaKemap_PA_NWTinset_20260226.png", plot = sk_map, width = 10, height = 8, dpi = 600)
