###################################
## 04_mapping_presence_absence.R
## Generating presence-absence maps for target species
## using camera data for ungulates and camera and aru data for game birds
## input: presence-absence matrices (generated in species_tag_summaries script), nwt area polygons
## output: presence-absence maps
## Started on July 9 2025
## Created by Erin Tattersall
###################################


#### Environment set up ####
## Load required packages (should already be installed)

x <- c("sf",
       "ggmap",
       "raster",
       "ggspatial",
       "cowplot",
       "maptiles",
       "leaflet",
       "terra", 
       "ggplot2", 
       "tidyterra",
       "tidyverse",
       "data.table")



## install.packages(x) ## should already be installed ####
lapply(x, require, character.only = TRUE)

## set working directory to file with NWT shapefiles
getwd()
list.files("data")
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/NWT_GIS_Data")
list.files()


#### Load shapefiles as sf objects ####
## NWT_boundary 
nwt.boun <- st_read("NWT_boundary.shp")
## Convert to NWT Lambert Conformal Conic projection, which is  the GNWT Standard projection
st_crs(nwt.boun) #NAD 83
nwt.boun <- st_transform(nwt.boun, crs = 3580) # 3580 is the NWT Lambert
st_bbox(nwt.boun)

### Load Study area polygons as one shapefile: Thaidene Nene, SambaaK'e_Dinaga, Gameti shapefiles, Fort Smith, Norman Wells merged into one in 05_fire_variables.R ####
area_sf <- st_read("NWTBM_6studyArea_polygons.shp")
st_crs(area_sf) # NAD 83 - NWT Lambert

## Load NWT fire data (note - adding in fire data considerably slows down mapping, especially for faceted plots)
list.files("C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/nrcan_nbac")
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/nrcan_nbac")
nwt_fire <- st_read("nwt_fires_nrcan_1972_2024.shp")
st_crs(nwt_fire) # NAD 83 - NWT Lambert


#### Load presence-absence matrices ####
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/wildtrax_download_camera")
list.files()

## Load presence-absence matrices for ungulates
pa_ungulates <- fread("NWTBM_ungulate_pa_matrix.csv")
# pa_birds_cam <- fread("NWTBM_bird_pa_matrix.csv") - not loaded yet - need to cross-check camera and ARU locations first (5 Aug 2025)

glimpse(pa_ungulates)


# Convert data to long format for easier plotting
pa_long <- pivot_longer(
  pa_ungulates,
  cols = c("Moose", "Barren_ground_Caribou", "Woodland_Caribou", "Bison", "Muskox"),
  names_to = "species",
  values_to = "presence"
)

glimpse(pa_long)

## Convert pa_long to an sf object
pa_long_sf <- st_as_sf(pa_long, coords = c("longitude", "latitude"), crs = 4326) # WGS 84
class(pa_long_sf)
pa_long_sf <- st_transform(pa_long_sf, crs = 3580) # Transform to NWT Lambert projection

table(pa_long_sf$species) ## Remove underscores from caribou names
pa_long_sf$species[pa_long_sf$species == "Barren_ground_Caribou"] <- "Barren-ground caribou"
pa_long_sf$species[pa_long_sf$species == "Woodland_Caribou"] <- "Woodland caribou"



# Create a presence absence map facet by species
# Get ESRI basemap (e.g., "World_Imagery" or "World_Topo_Map")
basemap <- get_tiles(nwt.boun, provider = "Esri.WorldImagery", crop = TRUE, zoom = 8) # NWT boundary polygon used to get extent of basemap, zoom level can be adjusted
# note: higher resolution base imagery takes longer to download and display


#### Faceted plot displaying presence-absence for each ungulate species

## First create map of NWT boundary, protected areas
# Create label positions above each polygon
area_sf <- area_sf %>%
  mutate(label_pos = st_coordinates(st_point_on_surface(geometry))) %>%
  mutate(label_pos_y = label_pos[,2] + 80000,  # shift Y upward by 50,000 units
         label_pos_x = label_pos[,1] - 1000) # shift X left by 10,000 units

class(nwt_fire$YEAR) # numeric

win.graph() # open a new graphics window
p <- ggplot() +
  layer_spatial(basemap) + # add basemap
  #geom_sf(data = nwt_fire, aes(color = YEAR), size = 1.5) + # fire polygons - removed because it makes the pa maps messy (Aug 5 2025)
  geom_sf(data = nwt.boun, fill = NA, linewidth = 1, color = "black") +
  geom_sf(data = area_sf, fill = NA, linewidth = 1, color = "black") + #SA polygons
  geom_text(data = area_sf,
            aes(x = label_pos_x, y = label_pos_y, label = name),
            size = 6, color = "white") + # add study area names
  #scale_color_gradient(low = "yellow", high = "red") + # red gradient for more recent burns
  coord_sf(xlim = c(-1026000, 580000), ylim = c(8100000, 9360000), expand = FALSE) + # set limits to bounding box of NWT fire layer (plus a little extra buffer)
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(legend.title= element_text(size = 16))


p

# Next, add presence-absence data for a single species to this map (moose)
p.moose <- p +
  geom_sf(data = pa_long_sf %>% filter(species == "Moose"), # filter for Moose presence-absence
    aes(color = factor(presence)), size = 2) +
  scale_color_manual(values = c("0" = "black", "1" = "orange"), labels = c("Absent", "Present")) +
  # scale_shape_manual(values = c("0" = 4, "1" = 16),  # X for absent, filled circle for present
            #         labels = c("Absent", "Present")) +
  labs(
    title = "Presence/Absence of Moose across NWTBM Stations",
    x = "Longitude",
    y = "Latitude",
    color = "Presence"
  ) +
  theme(legend.position = "right")

p.moose

# Now try adding faceting by species
p.faceted <- ggplot() +
  layer_spatial(basemap) + # add basemap
  geom_sf(data = nwt.boun, fill = NA, linewidth = 1, color = "black") +
  geom_sf(data = area_sf, fill = NA, linewidth = 1, color = "black") + #SA polygons
  geom_sf(data = pa_long_sf %>% filter(presence == 0), aes(color = factor(presence)), size = 1.5) + # absence points smaller
  geom_sf(data = pa_long_sf %>% filter(presence == 1), aes(color = factor(presence)), size = 2) + # presence points larger
  scale_color_manual(values = c("0" = "white", "1" = "orange"), labels = c("Absent", "Present")) +
  facet_wrap(~ species) +
  labs(
    title = "Presence/Absence of Ungulate Species by Location",
    x = "Longitude",
    y = "Latitude",
    color = "Presence") +
  coord_sf(xlim = c(-1026000, 580000), ylim = c(8100000, 9360000), expand = FALSE) + # set limits to bounding box of NWT mainland boundary
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(legend.title= element_text(size = 16),
  strip.text = element_text(size = 16, face = "bold"))  # facet title styling


p.faceted


## Save faceted presence-absence plot
ggsave("C:/Users/tatterer.stu/OneDrive - UBC/Documents/02.PhD/02.DataAnalysis/maps_figures/Ungulate_PA_facet_2025Aug23.jpeg", plot = p.faceted, width = 10, height = 6)




##### Presence-absence faceted plot for game birds #####

## Load presence-absence matrices for game birds (cams only)
pa_birds_cam <- fread("NWTBM_bird_pa_matrix.csv")

glimpse(pa_birds_cam)


# Convert data to long format for easier plotting
birds_long <- pivot_longer(
  pa_birds_cam,
  cols = c("Rock_Ptarmigan", "Ruffed_Grouse", "Sharp_tailed_Grouse", "Spruce_Grouse", "Willow_Ptarmigan"),
  names_to = "species",
  values_to = "presence"
)

glimpse(pa_long)

## Convert pa_long to an sf object
birds_long_sf <- st_as_sf(birds_long, coords = c("longitude", "latitude"), crs = 4326) # WGS 84
class(birds_long_sf)
birds_long_sf <- st_transform(birds_long_sf, crs = 3580) # Transform to NWT Lambert projection

table(birds_long_sf$species) 

## Remove underscores from caribou names
birds_long_sf$species[birds_long_sf$species == "Rock_Ptarmigan"] <- "Rock Ptarmigan"
birds_long_sf$species[birds_long_sf$species == "Ruffed_Grouse"] <- "Ruffed Grouse"
birds_long_sf$species[birds_long_sf$species == "Sharp_tailed_Grouse"] <- "Sharp-tailed Grouse"
birds_long_sf$species[birds_long_sf$species == "Spruce_Grouse"] <- "Spruce Grouse"
birds_long_sf$species[birds_long_sf$species == "Willow_Ptarmigan"] <- "Willow Ptarmigan"



#### Faceted plot displaying presence-absence for each bird species

win.graph()
p.birds <- ggplot() +
  layer_spatial(basemap) + # add basemap
  geom_sf(data = nwt.boun, fill = NA, linewidth = 1, color = "black") +
  geom_sf(data = area_sf, fill = NA, linewidth = 1, color = "black") + #SA polygons
  geom_sf(data = birds_long_sf %>% filter(presence == 0), aes(color = factor(presence)), size = 1.5) + # absence points smaller
  geom_sf(data = birds_long_sf %>% filter(presence == 1), aes(color = factor(presence)), size = 2) + # presence points larger
  scale_color_manual(values = c("0" = "white", "1" = "orange"), labels = c("Absent", "Present")) +
  facet_wrap(~ species) +
  labs(
    title = "Presence/Absence of Game Birds by Location",
    x = "Longitude",
    y = "Latitude",
    color = "Presence") +
  coord_sf(xlim = c(-1026000, 580000), ylim = c(8100000, 9360000), expand = FALSE) + # set limits to bounding box of NWT mainland boundary
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(legend.title= element_text(size = 16),
        strip.text = element_text(size = 18, face = "bold"))  # facet title styling


p.birds
## Save faceted presence-absence plot for birds
ggsave("C:/Users/tatterer.stu/OneDrive - UBC/Documents/02.PhD/02.DataAnalysis/maps_figures/Bird_PA_facet_2025Aug23.jpeg", plot = p.birds, width = 10, height = 6)
