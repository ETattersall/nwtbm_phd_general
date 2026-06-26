####################################
## nbac_fire_data_bysite.R
## Exploring and extracting fire data by NWTBMP site (sensor cluster)
## Started on Feb 13 2026 (split from 04 script on May 15 2026)
## Created by Erin Tattersall
####################################

#### Environment set up ####
## Load required packages (should already be installed)

list.of.packages <- c("sf",
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

#### Load Fire History data ####
## Use fire data extracted for 20km around study areas
sa_20km_fires <- st_read("data/nrcan_nbac/NBAC_1972to2024_20250506_shp/NBAC_fires_by_study_area_20kmbuffer.shp")


## Create Year0 for sa_20km_fires and sa_poly (corresponding to LAST year of deployment)
## sa_20km_fires
sa_20km_fires <- sa_20km_fires %>%
  mutate(Year0 = case_when(
    str_detect(study_area, "Edéhzhíe") ~ "2022",
    str_detect(study_area, "FortSmith") ~ "2024",
    str_detect(study_area, "Gameti") ~ "2024",
    str_detect(study_area, "NormanWells") ~ "2024",
    str_detect(study_area, "SambaaK'e") ~ "2023",
    str_detect(study_area, "ThaideneNëné") ~ "2022",
    TRUE ~ NA_character_  # Default case if no match
  ))
class(sa_20km_fires$Year0)

sa_20km_fires$Year0 <- as.numeric(sa_20km_fires$Year0)
glimpse(sa_20km_fires)


## Create FireAge variable
sa_20km_fires$FireAge <- sa_20km_fires$Year0 - sa_20km_fires$YEAR
summary(sa_20km_fires$FireAge)
hist(sa_20km_fires$FireAge)

## how many (and which ones) are negative values?
neg.fire.age <- sa_20km_fires[sa_20km_fires$FireAge < 0, ]
table(neg.fire.age$study_area) # 14 Edehzhie, 14 ThaideneNene, 1 Sambaa K'e
## Can remove these since fire doesn't occur during deployment period


sa_20km_fires <- sa_20km_fires[!sa_20km_fires$FireAge < 0, ]
summary(sa_20km_fires$FireAge)


##### Fire data buffered by site ####

## Read in site polygons and 500m site buffers from R/03_location_data.R
site_polygons <- st_read("data/sensor_locations/nwtbm_sensor_sites.gpkg")
sites_500m <- st_read("data/sensor_locations/nwtbm_sensor_sites_500mbuffer.gpkg")

glimpse(sites_500m) ## area of 500m site buffer already calculated in sq. m and sq. km

## Then extract fire for site buffer areas (using sa_20km_fires)
fires_500m_buffer <- st_intersection(sa_20km_fires, sites_500m)

glimpse(fires_500m_buffer) ## 161 rows - not all sites contain fire data (expected)
crs(fires_500m_buffer) # check CRS of fire data within buffers


## Quick summary of fire years represented
summary(fires_500m_buffer$YEAR) ## 1972-2023, median 2004
hist(fires_500m_buffer$YEAR) 


plot(fires_500m_buffer["YEAR"]) # map of fire years within 500m buffer around camera locations

## Not all sensor locations have fire history data within 500m buffers. Some locations have multiple fire polygons within the buffers
length(unique(fires_500m_buffer$site)) # 130 out of 231 sites have fire history data within 500m buffer

## Which FireAges are 0? Need to make sure sensors were still deployed when fire occurred
## Note: Fort Smith fires were in 2023, but deployment ended in 2024, so fires during the deployment have age = 1
FireAge0 <- fires_500m_buffer %>% 
  filter(FireAge == 0) %>% 
  select(study_area, site, HS_SDATE) ## all in Sambaa K'e in May 2023. Sensors were retrieved in March, so these can be discarded.

fires_500m_buffer <- fires_500m_buffer %>% 
  filter(FireAge > 0)

## Where fire geometries overlap, or one is contained within another, the most recent should be kept
## Check for polygon overlap or containment
fires_intersect <- st_intersects(fires_500m_buffer)
any(lengths(fires_intersect) > 1) #TRUE - there are polygons that overlap others 
sum(lengths(fires_intersect) > 1) ##58 geometries intersect

## Plotting intersecting polygons
plot(st_geometry(fires_500m_buffer), border = 'grey')
plot(
  st_geometry(fires_500m_buffer[lengths(fires_intersect) > 1, ]),
  col = 'red', add = TRUE
)


## Where fire polygons overlap, keep the polygon of the most recent fire
fires_500m_cleaned <- fires_500m_buffer %>%
  group_by(study_area, site) %>%
  group_modify(~ {
    
    df <- .x %>%
      arrange(FireAge)  # most recent first if smaller = newer
    
    clean_geoms <- vector("list", nrow(df))
    
    for (i in seq_len(nrow(df))) {
      
      current_geom <- df$geometry[i]
      
      if (i > 1) {
        newer_union <- st_union(df$geometry[1:(i - 1)])
        current_geom <- st_difference(current_geom, newer_union)
      }
      
      clean_geoms[[i]] <- current_geom
    }
    
    df$geometry <- st_sfc(do.call(c, clean_geoms), crs = st_crs(.x))
    df <- df[!st_is_empty(df), ]
    
    df
  }) %>%
  ungroup()



glimpse(fires_500m_cleaned)

#### Proportion of burned area within 500m buffer ####
## Calculate proportion burned for each fire polygon (some sites with multiple polygons)
fires_500m_cleaned <- fires_500m_cleaned %>% 
  mutate(burned_area_m2 = st_area(geometry)) %>% # calculate area of combined fire polygons in m^2
  mutate(proportion_burned_500m = as.numeric(burned_area_m2/site_area)) %>%  # calculate proportion of burned area within 500m buffer (convert to numeric to avoid units issues
  st_drop_geometry() # drop geometry for easier joining with sites_polygons

str(fires_500m_cleaned$site) ## 127 fire polygons
length(unique(fires_500m_cleaned$site)) ## 107 unique sites (124 do not have fire data)

## add proportion_burned_m2 to site_polygons for 500m buffers, adding a 0 value for locations with no fire history data within the buffer - FireAge should stay NA
site_fire_data <- sites_500m %>%
  left_join(
    fires_500m_cleaned %>%
      select(study_area, site, FireAge, proportion_burned_500m),
    by = c("study_area", "site") ## join 500m burn by site to match fire data to camera sites
  ) %>% 
  mutate(proportion_burned_500m = ifelse(is.na(proportion_burned_500m), 0, proportion_burned_500m))

glimpse(site_fire_data) ## check that the new columns have been added correctly. sf object with 262 rows and 7 columns (including geometry)
hist(site_fire_data$proportion_burned_500m) ## bimodal distribution - many sites (>150) with 0 burn, and some up to 100%
summary(site_fire_data) ## 124 NA FireAges (no fires in those sites)
length(unique(site_fire_data$site)) ## all sites accounted for

### Summarize proportion burned around each site (should add to 1 or less)
sum_burned <- site_fire_data %>% 
  group_by(site) %>% 
  summarise(sum_burned = sum(proportion_burned_500m))
summary(sum_burned)


## Some sites have multiple ages represented within their polygon (duplicated sites in site_fire_data)
## For each site with multiple fires, summarize FireAge and proportion of the burn for each fire at that site
multifires <- site_fire_data %>%
  group_by(site) %>%
  filter(n()>1) %>%
  arrange(site, FireAge) %>% 
  ungroup() #37 sites with multiple fires

hist(multifires$proportion_burned_500m, breaks = 10) # 14/37 fires < 0.1 proportion burned
hist(multifires$FireAge)

## What is the difference in proportions in sites with multiple fires?
diff_by_site <- multifires %>%
  group_by(site) %>%
  summarise(
    max_prop = max(proportion_burned_500m, na.rm = TRUE),
    min_prop = min(proportion_burned_500m, na.rm = TRUE),
    diff_prop = max_prop - min_prop,
    max_year = max(FireAge, na.rm = TRUE),
    min_year = min(FireAge, na.rm = TRUE),
    diff_year = max_year - min_year,
    .groups = "drop"
  )
summary(diff_by_site$diff_prop)
win.graph()
hist(diff_by_site$diff_prop, breaks = 10)
## 37 sites total, 3 sites have a difference < 0.1. 

hist(diff_by_site$diff_year) ## Most fires are young (<20 years)

## Ismael recommended assigning sites the most recent fire age if the proportion of the recent fire was above a certain threshold.
## I will use a threshold of 10% - that is, if the most recent fire is larger than 10% proportion burned, the site will be assigned the FireAge of the most recent fire.

site_fire_age <- site_fire_data %>%
  arrange(study_area, site, FireAge) %>% ## order fires by most recent
  group_by(study_area, site) %>% ## group by site
  slice(
    {
      idx <- which(proportion_burned_500m >= 0.10) ## selects all fires at a site with prop >= 0.1
      if (length(idx) > 0) idx[1] else 1 ## if there is at least one fire with prop >= 0.1, pick the most recent of these. If not (all fires have tiny proportions), pick the most recent
    }
  ) %>%
  ungroup()
    
## site_fire_age contains propburned and fireage for each site (with 0s and NAs, respectively, for sites with no fire)

## Save site fire data!!
write.csv(site_fire_age, "data/nrcan_nbac/nwtbm_sites_fire_prop_age_20260515.csv")

#### Fort Smith sites that burned during deployment (fireage = 1) ####
fs_fire2023 <- site_fire_age %>%
  filter(FireAge ==1) ## Only 5 sites

## Find fs_fire2023 sites in multifire (need to determine age and proportion prior to burn date)
fs_fire2023_sites <- fs_fire2023$site

fs_fire2023_multifires <- multifires %>% filter(site %in% fs_fire2023_sites)

## One site (SRL-324) has multiple fires (1, 9, and 19 years). Prop burned of 9 and 19 years is TINY (0.0004 and 0.0006, respectively). Based on above rule, we pick the most recent (9 year, column 2)
fs_fire2023_multifires <- fs_fire2023_multifires[2, ]


## Add columns to fs_fire2023 for pre2023_fireage and pre2023_propburned
fs_fire2023$pre2023_FireAge <- rep(NA, nrow(fs_fire2023))
fs_fire2023$pre2023_proportion_burned_500m <- rep(NA, nrow(fs_fire2023))

## Populate 3 rows in fs_fire2023 pre2023 columns
# Identify multifire sitess in fs_fire2023 (returns a vector of the position of matches in fs_fire2023)
idx <- match(fs_fire2023$site, fs_fire2023_multifires$site)

# populate pre2023 columns based on position in idx
fs_fire2023$pre2023_FireAge <- fs_fire2023_multifires$FireAge[idx]
fs_fire2023$pre2023_proportion_burned_500m <-
  fs_fire2023_multifires$proportion_burned_500m[idx]

## Also need date of 2023 fires from fires_500m_cleaned (though this should be determined by camera images, if possible)

fs_2023firedates <- fires_500m_cleaned %>% 
  filter(site %in% fs_fire2023_sites) %>% 
  filter(YEAR == 2023)
## Add AG_SDATE, as a rough indicator
idz <- match(fs_fire2023$site, fs_2023firedates$site)
fs_fire2023$fire_sdate2023 <-
  fs_2023firedates$AG_SDATE[idz]

## Save Fort Smith 2023 fire data
write.csv(fs_fire2023, "data/nrcan_nbac/FortSmith_2023firedata_sites.csv")


##### Plotting Fire Data ####
## Histogram of proportion of burned area within 500m buffers - proportion burned on the x-axis (binned to 0.1 intervals), frequency on the y-axis
hist_burned_500m <- site_fire_age %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = proportion_burned_500m)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black") + ## Binned to 0.1 intervals
  labs(title = "Distribution of Proportion of Burned Area within 500m Buffer of Sensor Sites",
       x = "Proportion of Burned Area",
       y = "Count of Sites") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16))

win.graph()
hist_burned_500m
## save
ggsave("figures/fire_explore/propburned_sites_500mbuffer_20260515.png", hist_burned_500m, width = 12, height = 8, dpi = 300)

## facet by study area

#500m buffer
hist_burned_sa_500m <- site_fire_age %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = proportion_burned_500m)) +
  geom_histogram(binwidth = 0.1, fill = "orange", color = "black") + ## Binned to 0.1 intervals
  facet_wrap(~ study_area) +
  labs(title = "Distribution of Proportion of Burned Area within 500m Buffer of Sensor Sites",
       x = "Proportion of Burned Area",
       y = "Count of Sites") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16))

win.graph()
hist_burned_sa_500m
## save
ggsave("figures/fire_explore/propburned_sites_500mbuffer_studyarea_20260515.png", hist_burned_sa_500m, width = 12, height = 8, dpi = 300)


#### Fire Age at each site ####
## Faceted plot of Fire Age for 500m buffer
fireage_500 <- site_fire_age %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = FireAge)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") + ## Binned to 10 years to reduce gaps in data
  facet_wrap(~ study_area) +
  labs(title = "Burn Age by Study Area (500m buffer)",
       x = "Burn Age",
       y = "Count of Sites") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

win.graph()
fireage_500
## save
ggsave("figures/fire_explore/fireages_byarea_sites_500mbuffer_20260515.png", fireage_500, width = 12, height = 8, dpi = 300)

## Not faceted
fireage_all_500 <- site_fire_age %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = FireAge)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") + ## Binned to 10 years to reduce gaps in data
  labs(title = "Burn Age (500m buffer)",
       x = "Burn Age",
       y = "Count of Sites") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

win.graph()
fireage_all_500
## save
ggsave("figures/fire_explore/fireages_sites_500mbuffer_20260515.png", fireage_all_500, width = 12, height = 8, dpi = 300)


