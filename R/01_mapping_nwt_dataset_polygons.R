####################################
## mapping_nwt_dataset_polygons.R
## Mapping the study areas for each dataset
## Started on Nov 22 2024
## Created by Erin Tattersall
####################################


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
       "ggspatial")



##### install.packages(x) ## should already be installed ####
lapply(x, require, character.only = TRUE)

## set working directory to file with NWT shapefiles
getwd()
list.files("data")
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/NWT_GIS_Data")
list.files()


#### Load NWT_boundary, Thaidene Nene, SambaaK'e_Dinaga, Gameti shapefiles, Fort Smith, Norman Wells polygons ####
nwt.boun <- vect("NWT_boundary.shp")
tdn.polygon <- vect("ThaideneNene.shp")
fs.polygon <- vect("FortSmith2022_polygon.kml")
sk.dn <- vect("SambaaK'e_Dinaga.shp")
gam.polygon <- vect("Gameti2023.shp")
nw.polygon <- vect("NormanWells2022_polygon.kml")
ede.polygon <- vect("Edehzhie.shp")


## Load NWT fire history
# nwt.fire <- 

#### Sambaa K'e and Dinaga will need to be split up ####
plot(sk.dn)
sk.dn$Name ##only want Sambaa K'e
sk.polygon <- sk.dn[sk.dn$Name == "Sambaa K'e"]
plot(sk.polygon)

#### Plot polygons, coloured for data sharing permissions ####
# Yes: TDN, Sambaa K'e, Fort Smith (light green)
# In negotiation: NW, Edehzhie, Gameti (sienna 1, aka light orange)

# First mapping attempt in ggplot (but I wanted a basemap)
# win.graph()
# ggplot() +
#   geom_spatvector(data = nwt.boun, fill ="lightgrey") +
#   geom_spatvector(data = tdn.polygon, fill = "lightgreen") +
#   geom_spatvector(data = sk.polygon, fill = "lightgreen") +
#   geom_spatvector(data = fs.polygon, fill = "lightgreen") +
#   geom_spatvector(data = gam.polygon, fill = "sienna1") +
#   geom_spatvector(data = nw.polygon, fill = "sienna1") +
#   geom_spatvector(data = ede.polygon, fill = "sienna1") +
#   theme_classic()




## testing leaflet basemap - I want to use Esri.WorldImagery
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  setView(lng = -118.8, lat = 64, zoom = 4.5) #%>% ## obtained centre from trial and error (couldn't figure out extracting centroid coordinates)


## mapping SpatVector polygons
data.map <- plet(
    x = c(tdn.polygon, sk.polygon, fs.polygon, ede.polygon, nw.polygon, gam.polygon, nwt.boun), # vector of all polygons
     col = c("lightgreen", "lightgreen", "lightgreen","orange", "orange","orange", "none"),# green for 'Yes', orange for 'In negotiation', none for boundary
     fill = 1, ## fill color should be opaque
    legend = NULL, # suppress legend
     tiles = "Esri.WorldImagery") #basemap
data.map

## Still want to label polygons, remove legend (names polys by X's), add a scale


