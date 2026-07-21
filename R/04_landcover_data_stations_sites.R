########################################
## 04_landcover_data_stations_sites.R
## Extracting landcover data at stations and sites
## Started on July 20 2026
## Created by Erin Tattersall
########################################


#### Environment set up ####
## Load required packages (should already be installed)

list.of.packages <- c("sf",
                      "lwgeom",
                      "data.table",
                      "tidyverse",
                      "dplyr",
                      "stars",
                      "ggspatial",
                      "cowplot",
                      "terra",
                      "ggplot2", 
                      "tidyterra", 
                      "ggspatial",
                      "viridis",
                      "corrplot",
                      "kableExtra",
                      "lubridate",
                      "purrr",
                      "ggplotify")



# A check to see which ones I have and which are missing
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Code which tells R to install the missing packages
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

## Read in station and sites with 500m buffers from R/03_location_data.R
stations_500m <- vect("data/sensor_locations/nwtbm_sensor_locations_500mbuffer.gpkg")
sites_500m <- vect("data/sensor_locations/nwtbm_sensor_sites_500mbuffer.gpkg")

stations_500m ## EPSG 3580
sites_500m ## EPSG 3580

## Load in Landcover Canada 2020 raster data (downloadable here: https://open.canada.ca/data/en/dataset/ee1580ab-a23d-4f86-a09b-79763677eb47)
list.files("data/LCC_2020")
lcc <- rast("data/LCC_2020/landcover-2020-classification.tif")
lcc ## projection = Canada Atlas Lambert (EPSG 3979)
summary(lcc) ## values 1-19 corresponding to pixel values in the class index (ClassIndex_v2.pdf)

## Reproject stations and sites to crop rasters
stns500_canlam <- project(stations_500m, crs(lcc))

sites500_canlam <- project(sites_500m, crs(lcc))

glimpse(stns500_canlam)


# # Crop raster to stations and sites
# lcc_stns <- mask(crop(lcc, stns500_canlam), stns500_canlam) # crop lcc to stns, then mask out values outside of buffers
# lcc_sites <- mask(crop(lcc, sites500_canlam), sites500_canlam)

# ## Adding LCC attributes from csv (converted the table from pdf to csv manually)
# lcc_atts <- read.csv("data/LCC_2020/ClassIndex_v2.csv") %>%
#   separate_wider_delim(RGB,
#                        delim = "-",
#                        names = c("R", "G", "B")) %>%  ## separating RGB values for use in plotting class colors
#   mutate(hex = rgb(R,G,B, maxColorValue = 255)) ## creating a column indicating the correct mapping color based on RGB values
# 
# ## Assigning landcover classes to lcc raster as levels and colors based on pixel values
# cats <- data.frame(value = lcc_atts$pixel_values, cover = lcc_atts$landcover_class)
# coltab <- data.frame(value = lcc_atts$pixel_values, cols = lcc_atts$hex)
# 
# ## Apply levels and colors to list of lcc data by area
# levels(lcc_stns) <- cats
# coltab(lcc_stns)  <- coltab
# 
# levels(lcc_sites) <- cats
# coltab(lcc_sites) <- coltab
# lcc_sites



####### Extract lcc around stations ##########
lcc_extract_stns <- extract(lcc, stns500_canlam)
glimpse(lcc_extract_stns)

## Calculate proportions of each category (still named numerically)
props_stn <- lcc_extract_stns %>%
    count(ID, Canada2020) %>%
  group_by(ID) %>%
  mutate(prop = n / sum(n)) %>%
  pivot_wider(
    names_from = Canada2020,
    values_from = prop,
    values_fill = 0
  )


## Add station attributes
station_info <- stns500_canlam |>
  as.data.frame() |>
  select(location, study_area) |>
  mutate(ID = row_number())

props_stn <- props_stn |>
  left_join(station_info, by = "ID")


summary(props_stn)

## Lump similar landcover classes to simplify (see the LCC attribute table ClassIndex_v2.csv):
# 1-2: conifer forest
# 8,11: shrubland
# 10, 12: grassland
#13, 16: barrenlands (no group 13 at stations)

## Also rename at the same time
props_lumped_stn <- props_stn |> 
  mutate(conifer = `1`+ `2`) |> 
  mutate(deciduous = `5`) |> 
  mutate(mixed = `6`) |> 
  mutate(shrubland = `8` + `11`) |> 
  mutate(grassland = `10` + `12`) |> 
  mutate(barren = `16`) |> 
  mutate(wetland = `14`) |> 
  mutate(water = `18`) |> 
  mutate(urban = `17`) |> 
  select(ID, study_area, location, conifer, deciduous, mixed, shrubland, grassland, barren, wetland, water, urban) # rearrange and drop n column

glimpse(props_lumped_stn)

summary(props_lumped_stn)


## Need one row per landcover variable for each station - sum all the rows of the same location
props_lumped_stn <- props_lumped_stn |> 
group_by(ID, study_area, location) |> 
  summarise(
    across(
      conifer:urban,
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

glimpse(props_lumped_stn)

## Do all landcover variables add to 1??
props_lumped_stn <- props_lumped_stn |> 
  mutate(
    total_cover = rowSums(across(conifer:urban), na.rm = TRUE))


summary(props_lumped_stn$total_cover)
hist(props_lumped_stn$total_cover)  # confirm that all add to 1 across rows :)
glimpse(props_lumped_stn)

### plot histograms of landcover classes

# Pivot to long format for plotting
props_lumped_stn_long <- props_lumped_stn %>%
  pivot_longer(
    cols = conifer:urban,
    names_to = "landcover",
    values_to = "proportion"
  )

lcc_stns_hist <- ggplot(props_lumped_stn_long,
       aes(x = proportion)) +
  geom_histogram(
    bins = 20,
    color = "black",
    fill = "steelblue"
  ) +
  facet_wrap(~ landcover, scales = "free_y") +
  labs(
    x = "Landcover Proportion",
    y = "Count of locations",
    title = "Distribution of Station Landcover Proportions by Category"
  ) +
  theme_bw()

win.graph()
lcc_stns_hist

## save the histogram
ggsave("figures/LCC/proportion_station_landcover_byclass.png", lcc_stns_hist, width = 12, height = 8, dpi = 300)


## Save props landcover independently of other covariates (backup copy)
write.csv(props_lumped_stn, "data/LCC_2020/nwtbm_stations_landcover_lumped_summary.csv")

#### Add to sensor covariate table ####
sensors <- read.csv("data/nwtbm_sensor_covariate_data.csv")


## Sensor level data (location)
glimpse(sensors)

##remove X and X.1 column
sensors <- sensors |> select(-X, -X.1)
glimpse(props_lumped_stn)

props_lumped_stn <- props_lumped_stn |> 
  select(-study_area, -ID, -total_cover) ## remove redundant or unnecessary columns from props df

sensors_lcc <- sensors |> 
  left_join(props_lumped_stn, by = "location")

glimpse(sensors_lcc)


### Save the covariate df
write.csv(sensors_lcc, "data/nwtbm_sensor_covariate_data.csv")


############ Extract lcc by site ###########
lcc_extract_sites <- extract(lcc, sites500_canlam)
glimpse(lcc_extract_sites)

glimpse(sites500_canlam)

## Calculate proportions of each category (still named numerically)
props_site <- lcc_extract_sites %>%
  count(ID, Canada2020) %>%
  group_by(ID) %>%
  mutate(prop = n / sum(n)) %>%
  pivot_wider(
    names_from = Canada2020,
    values_from = prop,
    values_fill = 0
  )


## Add site attributes
site_info <- sites500_canlam |>
  as.data.frame() |>
  select(site, study_area) |>
  mutate(ID = row_number())

props_site <- props_site |>
  left_join(site_info, by = "ID")


summary(props_site)

## Lump similar landcover classes to simplify (see the LCC attribute table ClassIndex_v2.csv):
# 1-2: conifer forest
# 8,11: shrubland
# 10, 12: grassland
#13, 16: barrenlands (no group 13 at stations or sites)

## Also rename at the same time
props_lumped_site <- props_site |> 
  mutate(conifer = `1`+ `2`) |> 
  mutate(deciduous = `5`) |> 
  mutate(mixed = `6`) |> 
  mutate(shrubland = `8` + `11`) |> 
  mutate(grassland = `10` + `12`) |> 
  mutate(barren = `16`) |> 
  mutate(wetland = `14`) |> 
  mutate(water = `18`) |> 
  mutate(urban = `17`) |> 
  select(ID, study_area, site, conifer, deciduous, mixed, shrubland, grassland, barren, wetland, water, urban) # rearrange and drop n column

glimpse(props_lumped_site)

summary(props_lumped_site)


## Need one row per landcover variable for each site - sum all the rows of the same location
props_lumped_site <- props_lumped_site |> 
  group_by(ID, study_area, site) |> 
  summarise(
    across(
      conifer:urban,
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

glimpse(props_lumped_site)

## Do all landcover variables add to 1??
props_lumped_site <- props_lumped_site |> 
  mutate(
    total_cover = rowSums(across(conifer:urban), na.rm = TRUE))


summary(props_lumped_site$total_cover)
hist(props_lumped_site$total_cover)  # confirm that all add to 1 across rows :)
glimpse(props_lumped_site)

### plot histograms of landcover classes

# Pivot to long format for plotting
props_lumped_site_long <- props_lumped_site %>%
  pivot_longer(
    cols = conifer:urban,
    names_to = "landcover",
    values_to = "proportion"
  )

lcc_sites_hist <- ggplot(props_lumped_site_long,
                        aes(x = proportion)) +
  geom_histogram(
    bins = 20,
    color = "black",
    fill = "steelblue"
  ) +
  facet_wrap(~ landcover, scales = "free_y") +
  labs(
    x = "Landcover Proportion",
    y = "Count of locations",
    title = "Distribution of Site Landcover Proportions by Category"
  ) +
  theme_bw()

win.graph()
lcc_sites_hist

## save the histogram
ggsave("figures/LCC/proportion_site_landcover_byclass.png", lcc_sites_hist, width = 12, height = 8, dpi = 300)


## Save props landcover independently of other covariates (backup copy)
write.csv(props_lumped_site, "data/LCC_2020/nwtbm_sites_landcover_lumped_summary.csv")

#### Add to site covariate table ####
sites <- read.csv("data/nwtbm_sites_covariate_data.csv")


## site level data (location)
glimpse(sites)

##remove X and X.1 column
sites <- sites |> select(-X, -X.1)
glimpse(props_lumped_site)

props_lumped_site <- props_lumped_site |> 
  select(-study_area, -ID, -total_cover) ## remove redundant or unnecessary columns from props df

sites_lcc <- sites |> 
  left_join(props_lumped_site, by = "site")

glimpse(sites_lcc)


### Save the covariate df
write.csv(sites_lcc, "data/nwtbm_sites_covariate_data.csv")

