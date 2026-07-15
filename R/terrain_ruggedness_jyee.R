######################################
## terrain_ruggedness_jyee.R
## Generating terrain ruggedness for NWT stations and sites using the terra package
## Started by Jayden Yee, modified by Erin Tattersall
## July 2026
######################################

x <- c("readr",
       "dplyr",
       "elevatr",
       "terra",
       "sf")

lapply(x, require, character.only = TRUE)

## Load sensor point locations
sensors <- read_csv("data/sensor_locations/nwtbm_allsensor_locations_20260506.csv")
# rename columns for compatibility with elevatr functions
sensors <- as.data.frame(sensors)
coordinates_df <- sensors |>
  rename("x" = "longitude", "y" = "latitude") |>
  select(x, y)


# Returns a raster with Terrain Ruggedness Index from a given zoom
# zoom corresponds to a ground resolution - see here: https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution
retrieve_TRI_rast <- function(zoom) {
  # gets raster elevation in meters for given coordinates x (longitude) and y (latitude)
  # locations must be a data.frame
  elevation_rast <- get_elev_raster(
    locations = coordinates_df,
    prj = 4326, # Standard unprojected GPS coordinate
    src = "aws", # AWS Terrain Tiles
    z = zoom)
  
  # calculates terrain characteristics from elevation raster
  # x must be a SpatRaster with elevation in meters
  # v is the terrain characteristic to compute
  # neighbours indicates how many neighbouring cells to use to compute slope or aspect with (either 4 or 8); 4 may be better for smoother surfaces while 8 may be better for rougher surfaces
  terrain_data <- terrain(
    x = elevation_rast,
    v = "TRI", # Terrain Ruggedness Index
    unit = "degrees",
    neighbours = 8
  )
  return(terrain_data)
}

# Generating TRI at Zoom 6 - 1223 m resolution, at 60 deg lat (approximating the area around a site)
terrain_data_6z <- retrieve_TRI_rast(6)

# Generating TRI at Zoom 8 - 305 m resolution at 60 deg lat (approximating the area around a station)
terrain_data_8z <- retrieve_TRI_rast(8)

## Try generating TRI at zoom 10 - 76.4 m resolution at 60 deg lat
terrain_data_10z <- retrieve_TRI_rast(10)

# The range of index values and corresponding meaning:
# 0-80 m - level surface
# 81-116 m - nearly level surface
# 117-161 m - slightly rugged surface
# 162-239 m - intermediately rugged surface
# 240-497 m - moderately rugged surface
# 498-958 m - highly rugged surface
# 959-4367 m - extremely rugged surface

plot(terrain_data_6z, main = "Terrain Ruggedness Index (6 zoom)")
plot(terrain_data_8z, main = "Terrain Ruggedness Index (8 zoom)")


## writing raster data to tif files
writeRaster(terrain_data_6z,
            filename = "outputs/TRI_data_6z.tif",
            overwrite = TRUE)
writeRaster(terrain_data_8z,
            filename = "outputs/TRI_data_8z.tif",
            overwrite = TRUE)


## Extract terrain ruggedness at stations and sites using extract function
## Requires polygons for site and station with 500m buffers 
stations_500m <- st_read("data/sensor_locations/nwtbm_sensor_locations_500mbuffer.gpkg")
sites_500m <- st_read("data/sensor_locations/nwtbm_sensor_sites_500mbuffer.gpkg")


## Extract mean TRI surrounding stations, using TRI calculated with zoom 10
tri_stns500m <- terra::extract(terrain_data_10z,
                               stations_500m,
                               fun=mean)
class(tri_stns500m) # returns a matrix - add to sensor df
summary(tri_stns500m)

## Add mean TRI to sensors df 
sensors$tri_500m <- tri_stns500m[ ,1]


## Extract mean TRI surrounding sites, using TRI calculated with zoom 10
tri_sites500m <- terra::extract(terrain_data_10z,
                               sites_500m,
                               fun=mean)

## Save sites_500m as df and add tri data to that
sites <- as.data.frame(sites_500m) ## keeps spatial geometry, does not contain coordinates

sites$tri_500m <- tri_sites500m[ ,1]


## Save terrain ruggedness in a csv for covariate data (will add other data to this)
write.csv(sensors, "data/nwtbm_sensor_covariate_data.csv")
write.csv(sites, "data/nwtbm_sites_covariate_data.csv")
