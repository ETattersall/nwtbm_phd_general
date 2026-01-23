###########################
## 05_study_area_maps.R
## Generating a map containing all study area polygons and station locations
## Created by Erin Tattersall
## July 4 2025 (though original script started in October 2024)
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
       "data.table")



## install.packages(x) ## should already be installed ####
lapply(x, require, character.only = TRUE)

## set working directory to file with NWT shapefiles
getwd()
list.files("data")
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/NWT_GIS_Data")
list.files()

## Load station location shapefile
stn_locations <- vect("NWTBM_stations_locations_May302025.shp")
class(stn_locations)

# Convert stn_locations to sf object
stn_locations_sf <- st_as_sf(stn_locations)

#### Load NWT_boundary, Thaidene Nene, SambaaK'e_Dinaga, Gameti shapefiles, Fort Smith, Norman Wells polygons ####
nwt.boun <- vect("NWT_boundary.shp") 
tdn.polygon <- vect("ThaideneNene.shp")
fs.polygon <- vect("FortSmith2022_polygon.kml")
sk.dn <- vect("SambaaK'e_Dinaga.shp")
gam.polygon <- vect("Gameti2023.shp")
nw.polygon <- vect("NormanWells2022_polygon.kml")
ede.polygon <- vect("Edehzhie.shp")

crs(nwt.boun, proj = TRUE) ## NAD 83 - will need to be transformed to WGS 84 for leaflet


st_bbox(nwt.boun) # bounding box of NWT boundary polygon - should be in lat/long degrees, but looks more like UTM

#### Sambaa K'e and Dinaga will need to be split up ####
plot(sk.dn)
sk.dn$Name ##only want Sambaa K'e
sk.polygon <- sk.dn[sk.dn$Name == "Sambaa K'e"]
plot(sk.polygon)

## Save Sambaa K'e polygon for future use
writeVector(sk.polygon, "Sambaa_Ke_protected_area.shp", overwrite = TRUE) # save as shapefile

## Converting SpatVect objects to sf objects for leaflet
vect_list <- list(nwt.boun, tdn.polygon, fs.polygon, sk.polygon, nw.polygon, ede.polygon, gam.polygon)
vect_list_sf <- lapply(vect_list, st_as_sf) # convert to sf objects
## Check class
class(vect_list_sf[[1]]) # should be "sf" "data.frame"


## Check coordinates and CRS
lapply(vect_list_sf, st_crs) # Fort Smith, Norman Wells, and Gameti are in WGS 84, but others are NAD 83

## Transform all to WGS 84
sf_list_wgs <- lapply(vect_list_sf, function(x) st_transform(x, crs = 4326)) # 4326 is WGS 84

## Alternatively, this function checks the CRS and transforms if needed (but not if it is already WGS 84)

# transform_to_wgs84 <- function(sf_list) {
#   lapply(sf_list, function(sf_obj) {
#     current_crs <- st_crs(sf_obj)
#     
#     # If CRS is missing entirely
#     if (is.na(current_crs)) {
#       warning("CRS is missing. Please assign the correct original CRS before transforming.")
#       return(sf_obj)
#     }
#     
#     # If EPSG is missing, compare using WKT string
#     if (is.na(current_crs$epsg)) {
#       if (grepl("WGS 84", current_crs$wkt, ignore.case = TRUE)) {
#         return(sf_obj) # Already WGS 84
#       } else {
#         message("Transforming from unknown EPSG to EPSG:4326 based on WKT.")
#         return(st_transform(sf_obj, 4326))
#       }
#     }
#     
#     # If EPSG is known and not 4326, transform
#     if (current_crs$epsg != 4326) {
#       message(paste("Transforming from EPSG:", current_crs$epsg, "to EPSG:4326"))
#       return(st_transform(sf_obj, 4326))
#     }
#     
#     # Already in WGS 84
#     return(sf_obj)
#   })
# }



# sf_list_wgs2 <- transform_to_wgs84(vect_list_sf) # apply function to list of sf objects

## Check CRS of both sf lists (which should be the same)
lapply(sf_list_wgs, st_bbox) # should be in lat/long degrees --looks good


# lapply(sf_list_wgs2, st_bbox) # should be in lat/long degrees --looks good

#### Plot all stations together ####


## Rough plot
plot(stn_locations, asp = T, las = 1,
     xlab = "Longitude", ylab = "Latitude")


## Check CRS for station locations
st_crs(stn_locations_sf) # should be WGS 84 (EPSG:4326), because I set it that way. Is the range correct?
st_bbox(stn_locations_sf) # should be in lat/long degrees --looks good


#### Map polygons and station locations together####
## list area polygons (including nwt.boun)
# area_polys <- c(tdn.polygon, sk.polygon, fs.polygon, ede.polygon, nw.polygon, nwt.boun) --> works in plet but not leaflet
#class(area_polys)

## Previously used plet wrapper, but leaflet provides more control (according to Copilot)
# poly.map <- plet(
#   x = area_polys, # vector of all polygons
#   col = "none",# no fill colours for polygons
#   legend = NULL, # suppress legend
#   tiles = "Esri.WorldImagery") #basemap
# poly.map  



# stn.map <- plet(x = stn_locations, # add station locations
#        col = "orange", # colour for station locations
#        fill = 1, # fill colour should be opaque
#        cex = 4, # point size
#        legend = NULL, # suppress legend
#        tiles = "Esri.WorldImagery") #basemap

#### Combined map using leaflet functions to combine layers (add each polygon separately) ####

# data.map <- leaflet() %>% 
#   addProviderTiles(providers$Esri.WorldImagery) %>%
#   setView(lng = -118.8, lat = 64, zoom = 4.5) %>% # obtained centre from trial and error (couldn't figure out extracting centroid coordinates)
#   addPolygons(data = sf_list_wgs[[1]], fillColor = "none", color = "black") %>% # add polygons individually from list of sf objects 1 = nwt boundary
#   addPolygons(data = sf_list_wgs[[2]], fillColor = "none", color = "black") %>% # add Thaidene Nene polygon
#   addPolygons(data = sf_list_wgs[[3]], fillColor = "none", color = "black") %>% # add Fort Smith polygon
#   addPolygons(data = sf_list_wgs[[4]], fillColor = "none", color = "black") %>% # add Sambaa K'e polygon
#   addPolygons(data = sf_list_wgs[[5]], fillColor = "none", color = "black") %>% # add Norman Wells polygon
#   addPolygons(data = sf_list_wgs[[6]], fillColor = "none", color = "black") %>% # add Edehzhie polygon
#   addCircleMarkers(data = stn_locations_sf, color = "green3", radius = 1, fillOpacity = 1) # add station locations
# data.map




### Map with ggplot2 and ggspatial 


# Get ESRI basemap (e.g., "World_Imagery" or "World_Topo_Map")
basemap <- get_tiles(sf_list_wgs[[1]], provider = "Esri.WorldImagery", crop = TRUE, zoom = 8) # NWT boundary polygon used to get extent of basemap, zoom level can be adjusted
# note: higher resolution base imagery takes longer to download and display

win.graph() # open separate graphics window
gg_map <- ggplot() +
  layer_spatial(basemap) + # add basemap
  geom_sf(data = sf_list_wgs[[1]], fill = NA, linewidth = 2, color = "black") + # NWT boundary
  geom_sf(data = sf_list_wgs[[2]], fill = NA, linewidth = 2, color = "black") + # Thaidene Nene polygon
  geom_sf(data = sf_list_wgs[[3]], fill = NA, linewidth = 2, color = "black") + # Fort Smith polygon
  geom_sf(data = sf_list_wgs[[4]], fill = NA, linewidth = 2, color = "black") + # Sambaa K'e polygon
  geom_sf(data = sf_list_wgs[[5]], fill = NA, linewidth = 2, color = "black") + # Norman Wells polygon
  geom_sf(data = sf_list_wgs[[6]], fill = NA, linewidth = 2, color = "black") + # Edehzhie polygon
  geom_sf(data = sf_list_wgs[[7]], fill = NA, linewidth = 2, color = "black") + # Gameti polygon
  geom_sf(data = stn_locations_sf, aes(color = area), size = 1.5) + # station locations
  #coord_sf() + ## assumes all layers are the same CRS - not needed here because I transformed all to WGS 84
  labs(title = "NWT Station Locations and Study Areas",
       x = "Longitude",
       y = "Latitude",
       color = "Stations by Study Area") +
  theme(legend.position = "right") +
  scale_y_continuous(limits = c(60,70)) + # set latitude range
  theme_classic()

gg_map
## Save map as PNG
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd") # set working directory to save map (though shouldn't save it in git folder - move to 02.DataAnalysis/maps_figures folder)
ggsave("NWTBM_stations_map_May302025.png", plot = gg_map, width = 10, height = 8, dpi = 300)