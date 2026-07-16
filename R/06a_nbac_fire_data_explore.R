####################################
## nbac_fire_variable_explore.R
## Initial exploration of NBAC fire data for all camera locations
## Started on Feb 13 2026
## Created by Erin Tattersall
####################################


### Note: check working and root directories - some sections written when script was in the nwtbm_phd_ungulates Rproj

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


## Read in study area shapefiles - both regular and with 20km buffer (in study_area_spatial)

setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd_ungulates/data/study_area_spatial")
list.files()
sa_poly <- st_read("NWTBM_all_study_areas.shp")
sa_20km <- st_read("NWTBM_all_study_areas_20km_buffers.shp")
crs(sa_poly)
crs(sa_20km) # same projection



#### Load Fire History data ####
## Canada Fire History data between 1972-2024 from NRCan: https://cwfis.cfs.nrcan.gc.ca/datamart/metadata/nbac
## Also have NWT fire history data from GNWT website, but I think that only goes to 2023 (may be a more recent download?)
## Use NRCan data, since this is what Claudia also used
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd_ungulates/data/nrcan_nbac/NBAC_1972to2024_20250506_shp")
fire_history <- st_read("NBAC_1972to2024_20250506.shp")
head(fire_history)
## Check CRS
st_crs(fire_history) # NAD 83 - Canada Lambert Conformal Conic

summary(fire_history$YEAR) ## all years (1972-2024)

## Transform to NWT Lambert and crop fire history data to NWT boundary 
fire_history <- st_transform(fire_history, crs = 3580)

## First filter fire_history for fires in NT (though this still includes fires in Nunavut prior to it becoming a separate territory in 1999)
nwt_fires <- fire_history %>%
  filter(ADMIN_AREA == "NT") # filter for fires in NT

## save NWT fire data as a separate shapefile for faster loading in future
st_write(nwt_fires, "nwt_fires_1972to2024.shp")

### Extract fire data for 20km buffers. Use full fire history because a tiny corner of Fort Smith buffer is in Alberta (one FS station is ~900m from border)
sa_20km_fires <- st_intersection(fire_history, sa_20km)

## save 20km buffered fire data for later use
st_write(sa_20km_fires, "NBAC_fires_by_study_area_20kmbuffer.shp", append = FALSE)


## Calculate total area of study areas (will be needed later)
sa_poly$area_m2 <- st_area(sa_poly)


##### Extract fire data for study areas without buffers - not done yet ###
sa_fires_nobuffer <- st_intersection(sa_20km_fires, sa_poly)

## Remove fire_history from environment to save memory
rm(fire_history)

## return to base directory
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd_ungulates")


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

sa_20km_fires$FireAge <- sa_20km_fires$Year0 - sa_20km_fires$YEAR
summary(sa_20km_fires$FireAge)
hist(sa_20km_fires$FireAge)

## how many (and which ones) are negative values?
neg.fire.age <- sa_20km_fires[sa_20km_fires$FireAge < 0, ]
table(neg.fire.age$study_area) # 14 Edehzhie, 14 ThaideneNene, 1 Sambaa K'e
## Can remove these since fire doesn't occur during deployment period


sa_20km_fires <- sa_20km_fires[!sa_20km_fires$FireAge < 0, ]
summary(sa_20km_fires$FireAge)

#### Summary statistics ####

## Extract fire data for study areas without buffers
sa_fires_nobuffer <- st_intersection(sa_20km_fires, sa_poly)
glimpse(sa_fires_nobuffer)


## Summarize fire ages and sizes an by study area
sa_fire_stats <- sa_fires_nobuffer %>% 
  group_by(study_area) %>% 
  summarise(f_age_min = min(FireAge),
            f_age_mean = mean(FireAge),
            f_age_max = max(FireAge),
            f_size_min = min(ADJ_HA),
            f_size_mean = mean(ADJ_HA),
            f_size_max = max(ADJ_HA)) %>%
  st_drop_geometry()  # drop geometry for easier joining

## Proportion burned/unburned within each study area
pburn_sa <- sa_fires_nobuffer %>%
  group_by(study_area) %>% ## group all polygons by study area
  summarise(geometry = st_union(geometry)) %>%  # combine all fire polygons within same study area
  mutate(burned_area_m2 = st_area(geometry)) %>% # calculate area of combined fire polygons in m^2
  st_drop_geometry()  # drop geometry for easier joining

## Add area of sa from sa_poly to pburn_sa
pburn_sa <- pburn_sa %>%
  left_join(
    sa_poly %>% 
      dplyr::select(study_area, area_m2),
    by = "study_area")

## Add proportion of burned area to each sa
pburn_sa$prop_burned <- pburn_sa$burned_area_m2/pburn_sa$area_m2

## Now add prop_burned to sa_fire_stats
sa_fire_stats <- left_join(sa_fire_stats, 
                           pburn_sa %>% 
                             select(study_area, prop_burned),
                           by = "study_area")

## Arrange from most prop. burned to least
sa_fire_stats <- sa_fire_stats %>% arrange(desc(prop_burned))

### Save summary stats
write.csv(sa_fire_stats,"figures/fire_explore/fire_stats_by_study_area.csv")

## Bar plot of proportion burned by study area
glimpse(sa_fire_stats)

## convert prop_burned to numeric
sa_fire_stats$prop_burned <- as.numeric(sa_fire_stats$prop_burned)
glimpse(sa_fire_stats)

bar_burned <- sa_fire_stats %>%
  ggplot(aes(x = study_area, y = prop_burned)) +
  geom_bar(stat = "identity", fill = "orange", color = "black") +
  labs(title = "Proportion of Burned Area by Study Area",
       x = "Study Area",
       y = "Proportion of Burned Area") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(axis.text = element_text(size = 16))

win.graph()
bar_burned
## save
ggsave("figures/fire_explore/propburned_by_study_area_202604027.png", bar_burned, width = 12, height = 8, dpi = 300)


## Faceted plot of Burn Age by Study Area (20km buffer)
fireage_sa <- sa_20km_fires %>%
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = FireAge)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") + ## Binned to 10 years to reduce gaps in data
  facet_wrap(~ study_area) +
  labs(title = "Time Since Fire by Study Area",
       x = "Burn Age",
       y = "Number of Fires") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

win.graph()
fireage_sa
## save
ggsave("figures/fire_explore/fireages_byarea_20kbuffer_20260427.png", fireage_sa, width = 12, height = 8, dpi = 300)


### Fire Mapping: NWT and by Study Area ####

### Map of all NWT fire history plus sensor locations (including 500m buffers for visibility)
gg_nwt_fires <- ggplot() +
  geom_sf(data = nwt_fires, aes(color = YEAR), size = 0.5) + # all NWT fire polygons
  geom_sf(data = cams_500m_buffer, fill = NA, color = "blue", size = 0.5) + # 500m buffers around camera locations
  geom_sf(data = cam_locs_sf, color = "black", size = 1) + # camera locations
  scale_color_gradient(low = "yellow", high = "red") + # red gradient for more recent burns
  labs(title = "NWT Fire History (1972 - 2024) with Camera Locations",
       x = "Longitude",
       y = "Latitude",
       color = "Fire Year") +
  theme(legend.position = "right") +
  coord_sf(xlim = c(-1026000, 580000), ylim = c(8100000, 9360000), expand = FALSE) + # set limits to bounding box of NWT fire layer (plus a little extra buffer)
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(legend.title= element_text(size = 16))

gg_nwt_fires


### Faceted map of fire history for each study area

## Create each map separately (in a list), then plot together using plot_grid() (cowplot package)

## list of study areas
study_areas <- sa_poly$study_area

## Create a function to make one plot per study area
make_sa_plot <- function(sa) {
  
  # get bounding box for that study area (using sa_20km fires polygons)
  sa_bbox <- sa_20km_fires %>%
    dplyr::filter(study_area == sa) %>%
    summarise() %>%
    st_bbox()
  
  ggplot() +
    geom_sf(data = sa_20km_fires %>% filter(study_area == sa),
            aes(fill = FireAge), color = NA,
            size = 0.5) +
    
    geom_sf(data = cam_locs_sf %>% filter(study_area == sa),
            color = "black",
            size = 2) +
    
    scale_fill_gradient(low = "red", high = "yellow", name = "Time Since Fire") +
    
    coord_sf(xlim = c(sa_bbox["xmin"], sa_bbox["xmax"]),
             ylim = c(sa_bbox["ymin"], sa_bbox["ymax"]),
             expand = FALSE) +
    
    labs(title = sa,
         x = "Longitude",
         y = "Latitude") +
    
    theme_classic() +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      legend.position = "right"
    )
}


## Create list of ggplots
plot_list <- lapply(study_areas, make_sa_plot)


combined_plot <- plot_grid(plotlist = plot_list,
                           ncol = 3)   # adjust columns as needed

win.graph()
combined_plot


## Save the plot
save_plot(
  "figures/fire_explore/TimeSinceFire_bySA.jpeg",
  combined_plot,
  ncol = 3,
  nrow = 2,
  base_asp = 1.618,
  dpi = 300
)

#### Is there a relationship between fire age and fire size at the study area level?

fire_age_size <- sa_20km_fires %>% 
  st_drop_geometry() %>% # drop geometry for easier plotting
  ggplot(aes(x = FireAge, y = ADJ_HA)) +
  geom_point() +
  labs(title = "Relationship between Fire Age and Fire Size",
       x = "Fire Age",
       y = "Fire Size (HA)") +
  theme_classic() + 
  # increase size of title text, axis text, and facet titles
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(strip.text = element_text(size = 14)) # increase facet title size

win.graph()
fire_age_size

## add regression line with 95% confidence intervals to fire_age_size
fire_age_size <- fire_age_size + geom_smooth(method = "lm", se = TRUE, color = "purple")
fire_age_size

## save
ggsave("figures/fire_explore/fire_age_size_relationship_bystudyarea_202604027.png", fire_age_size, width = 12, height = 8, dpi = 300)

## Assessing relationship between fire age and fire size with a linear model
lm(ADJ_HA ~ FireAge, data = sa_20km_fires) %>% summary() ## Low R-squared and non-significant relationship


