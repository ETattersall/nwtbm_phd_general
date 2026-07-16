####################################
## climate_data_climr_jyee.R
## Extracting climate data for NWT stations and sites using the climr package
## Written by Jayden Yee, modified by Erin Tattersall
## Jun-July 2026
####################################



# # To install packages from github
# install.packages("remotes")
library(remotes)

# # May need to update packages and remove files for it to install correctly
# install_github("bcgov/climr")
library(climr)

# # To extract elevation data from point coordinates
# install.packages("elevatr")
library(elevatr)

library(readr)
sensors <- read_csv("data/sensor_locations/nwtbm_allsensor_locations_20260506.csv")

library(dplyr)
sensors <- sensors |>
  select(-...1) |>
  rename("lon" = "longitude", "lat" = "latitude") |>
  relocate(site, .before = location) |>
  relocate(lon, .before = lat)

sensors <- data.frame(sensors)

coordinates_df <- sensors |>
  rename("x" = "lon", "y" = "lat") |>
  select(x, y)

# Returns given data frame with elevation data and corresponding zoom
retrieve_elev_df <- function(df, zoom) {
  # Returns elevation in meters for each point
  # Must be a data frame, not a tibble. Requires the data frame to have x as 1st column (longitude) and y as 2nd column (latitude). 
  # prj is the Coordinate Reference System
  # src is the specified API to use 
  # z stands zoom. Higher zoom results in a smaller pixel size and higher accuracy but longer downloads. Please see "https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution" for full details of each zoom level. The default zoom (5) uses a large 4-5 km pixel.
  elevation <- get_elev_point(
    locations = coordinates_df,
    prj = 4326, # Standard unprojected GPS coordinate
    src = "aws", # AWS Terrain Tiles
    z = zoom)
  
  df_with_elevation <- df |>
    mutate(elev = elevation$elevation, 
           zoom = zoom,
           .after = "lat")
   
  return(df_with_elevation)
}

## Extracting elevation at zoom 10 - 76.4 m resolution
sensors <- bind_rows(retrieve_elev_df(sensors, 10))|>
  mutate(id = row_number(),
         .before = study_area)

###############################################################
##### What climate data are available. Please also share  #####
#####     a list of variables available, including        #####
#####             descriptions if possible,               #####
###############################################################

# List all methods available in the package
ls("package:climr")

# Lists available climate variables
list_vars()

# Variable descriptions
View(variables)

#'* There are 247 available climate variables. The climate data available are Heat:moisture index, frost-free period, Hargreaves climate moisture deficit, climate moisture index, Hargreaves reference evaporation, precipitation, precipitation as snow, temperature, and relative humidity*

###############################################################
#####      Figuring out what inputs are accepted          #####
###############################################################

library(stringr)

climate_vars <- variables |>
  filter(str_starts(Code_Element, "P") |
         (str_starts(Code_Element, "T") &
            Code_Element != "TD") |
           str_starts(Code_Element, "MA") |
           Code_Element == "MCMT") |>
  pull(Code)

# Lists all available years of observational climate data
list_obs_years()

# provides "scale-free" downscaled climate variables for user-specified locations
# xyz is a data frame that must have columns "lon," "lat," "elev" (meters), and "id."
# obs_years is the years to obtain observational climate data for
# obs_ts_dataset is the dataset to use for the observational climate data (options are "climatena" for the ClimateNA gridded time series (only goes to 2023) or "cru.gpcc" for the combined Climatic Research Unit TS dataset (for temperature) and Global Precipitation Climatology Centre dataset (for precipitation))
# vars are the variables to output
ds_data <- downscale(
  xyz = sensors,
  obs_years = 2020:2024,
  obs_ts_dataset = c("climatena", "cru.gpcc"),
  vars = climate_vars)

# The downscale() function always outputs the reference period "1961-1990," so I will filter it out
ds_data <- filter(ds_data, PERIOD != "1961_1990")

# Keep cru.gpcc only for the year 2024, as climatena is higher resolution
ds_data <- filter(ds_data, DATASET == "climatena" | PERIOD == 2024)

ds_data <- sensors |>
  left_join(ds_data)

# write_csv(variables, file = "outputs/climate_variables.csv")
write_csv(ds_data, file = "data/nwtbm_climate_data_elevzoom10.csv")

###############################################################
##### At what resolution (particularly temperature and    #####
#####     precipitation data between 2020 - 2024).        #####
###############################################################

# The Climatic Research Unit TS dataset (for temperature) and Global Precipitation Climatology Centre dataset (for precipitation)) both have a base resolution of 55km x 55km or finer
# The ClimateNA dataset has a base resolution of 1 km x 1 km or finer

# Change-factor downscaling of coarse-resolution (50-200km grid) monthly temperature and precipitation data from climate models or observational sources to high-resolution (800m grid);
# Elevation adjustment of temperature variables to provide scales finer than the high-resolution reference grid; and
# Calculating derived variables from the downscaled monthly temperature and precipitation variables.

#'*In summary, the accuracy of the output of downscale() is influenced by the resolution of the elevation data, the resolution of the climate dataset, and the accuracy of the point locations at the least*


### Reading in climate data to explore
ds_data <- read.csv("data/nwtbm_climate_data_elevzoom10.csv")

glimpse(ds_data) ## 4110 rows of 99 variables - 5 years of climate data at 822 stations


## Want to refine this only to relevant time periods for each study area

# Create a lookup table of deployment periods

# Deployment periods for each study area
dep_periods <- tibble(
  study_area = c("Edéhzhíe",
                 "ThaideneNëné",
                 "FortSmith",
                 "NormanWells",
                 "SambaaK'e",
                 "Gameti"),
  start_year = c(2021, 2021, 2022, 2022, 2022, 2023),
  end_year   = c(2022, 2022, 2024, 2024, 2023, 2024)
)


# Filter climate dataframe
climate_deps <- ds_data %>%
  left_join(dep_periods, by = "study_area") %>%
  filter(PERIOD >= start_year & PERIOD <= end_year) %>%
  select(-start_year, -end_year) # remove start and end year columns


## Summarize climate variables at each site (take the mean across sites) -- see last copilot conversation for solutions
## specify the climate variables - columns 12-99

clim_vars <- names(climate_deps)[12:99]

clim_site <- climate_deps |> 
  group_by(site, PERIOD) |> 
  summarise(
    across(all_of(clim_vars), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
    )


### Save climate data to covariate csvs (created in terrain_ruggedness_jyee.R script)
sensors <- read.csv("data/nwtbm_sensor_covariate_data.csv")
sites <- read.csv("data/nwtbm_sites_covariate_data.csv")


## Sensor level data (location)
glimpse(sensors)

##remove X and ...1 column
sensors <- sensors |> select(-X, -...1)
glimpse(climate_deps)

climate_deps <- climate_deps |> 
  select(-study_area, -site, -lon, -lat, -sensor_type) ## remove redundant columns from sensors df

sensors_clim <- sensors |> 
  left_join(climate_deps, by = "location")

## Site level data
glimpse(sites)

sites <- sites |> select(-X)

glimpse(clim_site)

sites_clim <- left_join(sites, clim_site, by = "site")

glimpse(sensors_clim)

glimpse(sites_clim)


### Save as covariate CSVs
write.csv(sensors_clim, "data/nwtbm_sensor_covariate_data.csv")
write.csv(sites_clim, "data/nwtbm_sites_covariate_data.csv")
