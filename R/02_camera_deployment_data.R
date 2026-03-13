###########################
## 02_camera_deployment_data.R
## Started on Mar 12 2026
## Created by Erin Tattersall
###########################


#### Environment set up #### 

## List of packages 
x <- c("sf",
       "terra", 
       "ggplot2", 
       "tidyterra", 
       "ggspatial",
       "wildrtrax",
       "tidyverse",
       "data.table",
       "plotly")

## install.packages(x) ## should already be installed ####
lapply(x, require, character.only = TRUE)


### Load WildTrax image set data
setwd("data/wt_camera_reports_allprojects")
list.files() ## want to load in all reports ending in "image_set_report.csv"
img_set_list <- list.files(pattern = "image_set_report.csv")
img_set_list ## 6 reports to load in (1 per project)

## Load in each report, add a column for the source file, and combine into one data frame
dep_df <- lapply(img_set_list, function(x) {
  df <- fread(x)
  df$source_file <- x
  return(df)
}) %>% bind_rows()



## Also want to load in all image reports
img_rep_list <- list.files(pattern = "image_report.csv")
## Load in each image report, add a column for the source file (can't yet combine into one df because some column classes differ)
img_df <- lapply(img_rep_list, function(x) {
  df <- fread(x)
  df$source_file <- x
  return(df)
})

glimpse(img_df) ## image_fire in the Sambaa K'e df is DNC (did not collect?), character class instead of logical. Convert to logical
table(img_df[[2]]$image_fire) ## all DNC - convert to FALSE
img_df[[2]]$image_fire <- as.logical(img_df[[2]]$image_fire)
glimpse(img_df)
## convert image_snow in Edehzhie to logical
img_df[[1]]$image_snow <- as.logical(img_df[[1]]$image_snow)

## Now combine all dfs in img_df into one biiiiiig df
img_df <- bind_rows(img_df)

## return to base directory
setwd("../../")

### Add a column for study area
dep_df <- dep_df %>%
  mutate(study_area = case_when(
    str_detect(source_file, "Edéhzhíe") ~ "Edéhzhíe",
    str_detect(source_file, "Fort_Smith") ~ "FortSmith",
    str_detect(source_file, "Gameti") ~ "Gameti",
    str_detect(source_file, "Norman_Wells") ~ "NormanWells",
    str_detect(source_file, "Sambaa_K'e") ~ "SambaaK'e",
    str_detect(source_file, "Thaidene_Nëné") ~ "ThaideneNëné",
    TRUE ~ NA_character_  # Default case if no match
  ))

table(is.na(dep_df$study_area)) ## all study area names copied correctly

img_df <- img_df %>%
  mutate(study_area = case_when(
    str_detect(source_file, "Edéhzhíe") ~ "Edéhzhíe",
    str_detect(source_file, "Fort_Smith") ~ "FortSmith",
    str_detect(source_file, "Gameti") ~ "Gameti",
    str_detect(source_file, "Norman_Wells") ~ "NormanWells",
    str_detect(source_file, "Sambaa_K'e") ~ "SambaaK'e",
    str_detect(source_file, "Thaidene_Nëné") ~ "ThaideneNëné",
    TRUE ~ NA_character_  # Default case if no match
  ))

table(img_df$study_area) ## all study area names copied correctly

## Remove source_file column
dep_df <- dep_df %>%
  select(-source_file)

img_df <- img_df %>%
  select(-source_file)

#### Explore image sets (or deployments) ####

## 722 rows - one per location? how many unique locations?
length(unique(dep_df$location)) ## 706 unique locations - which ones are duplicated?
length(unique(dep_df$image_set_id)) ## 722 - 16 locations that are duplicates of others
## Subset dep_df for duplicated locations
dup_locs <- dep_df[duplicated(dep_df$location) | duplicated(dep_df$location, fromLast = TRUE), ] ## Selecting all rows where the location is duplicated, either by starting at the top or the bottom of the df (which ensures inclusion of rows that appear more than twice)
# Group rows with the same location
dup_locs <- dup_locs[order(dup_locs$location), ]
## Keep only columns for study_area, location, image_set_id, image_set_start_date_time, and image_set_end_date_time
dup_locs <- dup_locs %>%
  select(study_area, location, image_set_id, image_set_start_date_time, image_set_end_date_time)

## Each image set represents a camera check - confirmed that all checks are successive with no date gaps
## In dep_df, merge rows with the same location by keeping the earliest start date and latest end date for each location, summarizing values for image_set_count_motion, image_set_count_timelapse, and image_set_count_total by summing them across rows with the same location, and keeping the first value for study_area and image_set_id
dep_df <- dep_df %>%
  group_by(study_area, location) %>%
  summarise(study_area = first(study_area), 
            image_set_id = first(image_set_id), 
            image_set_start_date_time = min(image_set_start_date_time), 
            image_set_end_date_time = max(image_set_end_date_time),
            image_set_count_motion = sum(image_set_count_motion),
            image_set_count_timelapse = sum(image_set_count_timelapse),
            image_set_count_total = sum(image_set_count_total)) %>%
ungroup()
# dep_df now has 706 rows, one per unique location!

glimpse(dep_df)

## Summarise minimum start time and maximum end time for each study area
dep_time_summary <- dep_df %>%
  group_by(study_area) %>%
  summarise(image_set_start_date_time = min(image_set_start_date_time),
            image_set_end_date_time = max(image_set_end_date_time))

## Earliest Edehzhie date doesn't look right (2020-01-01) - ENWA-O-9-2. Deployment data shows camera deployed Nov 8 2021 at 10:57. Retrieved on Nov 6 2022 at 10:33
## Similarly, earliest TDN date is (2021-03-03) - should be 2021-08-28


## Adjust start time for BMS-CRU-004-001 in TDN and start and end times for ENWA-O-9-2
dep_df <- dep_df %>%
  mutate(
    image_set_start_date_time = ymd_hms(image_set_start_date_time),
    image_set_end_date_time = ymd_hms(image_set_end_date_time)) %>%
  mutate(
    image_set_start_date_time = if_else(   #this is because the camera was turned on in March so start date is incorrect
      location == "BMS-CRU-004-01",
      ymd_hms("2021-08-28 12:00:00"), if_else(
        location == "ENWA-O-9-2",
        ymd_hms("2021-11-08 10:57:00"),
      image_set_start_date_time))) %>% 
  mutate(
    image_set_end_date_time = if_else(
      location == "ENWA-O-9-2",
      ymd_hms("2022-11-06 10:33:00"),
      image_set_end_date_time))

## Rerun dep_time_summary to check again
dep_time_summary <- dep_df %>%
  group_by(study_area) %>%
  summarise(image_set_start_date_time = min(image_set_start_date_time),
            image_set_end_date_time = max(image_set_end_date_time))
## Looks okay (Edehzhie did have a few deployments in Feb 2021)

## Note that for ENWA-O-9-2 the date and time for all species detections will be wrong!! (confirmed that detection phenology plot does have caribou detections in 2020, which is wrong)

glimpse(dep_df)
## Want study_area and location to be factors in both dep_df and img_df
dep_df$study_area <- as.factor(dep_df$study_area)
dep_df$location <- as.factor(dep_df$location)

glimpse(img_df)
img_df$study_area <- as.factor(img_df$study_area)
img_df$location <- as.factor(img_df$location)

## Save deployment data across projects
write.csv(dep_df, "data/all_projects_deployment_data_20260312.csv")

## Save dep_time_summary for easy referencing
write.csv(dep_time_summary, "data/all_projects_dep_period_summary_20260312.csv")


### Check out of range dates in img_df
table(img_df$image_fov)


## Calculate survey effort by creating a daily camera activity matrix
glimpse(dep_df)
class(dep_df$image_set_start_date_time)


## Create a daily camera activity lookup
#convert deployment start and end date and time columns to date objects
dep_df$start_date <- as.Date(dep_df$image_set_start_date_time, format = "%Y-%m-%d")

dep_df$end_date <- as.Date(dep_df$image_set_end_date_time, format = "%Y-%m-%d")

#create a column of number of days deployed
dep_df$days_active <- interval(dep_df$start_date, dep_df$end_date)/ddays(1)

hist(dep_df$days_active)

# Remove any deployments without end dates
tmp <- dep_df[is.na(dep_df$end_date)==F,]

# Create an empty list to store our days
daily_lookup <- list()

# Loop through the deployment dataframe and create a row for every day the camera is active
for(i in 1:nrow(tmp))
{
  if(ymd(tmp$start_date[i])!=ymd(tmp$end_date[i]))
  {
    daily_lookup[[i]] <- data.frame("date"=seq(ymd(tmp$start_date[i]), ymd(tmp$end_date[i]), by="days"), "location"=tmp$location[i])
  }
}

# Merge the lists into a dataframe
row_lookup <- bind_rows(daily_lookup)

# Remove duplicates - when start and end days are the same for successive deployments
row_lookup <- row_lookup[duplicated(row_lookup)==F,]

#### Plotting camera activity per study area


## are there NAs in any location, start date or end date?
table(is.na(dep_df$location)) #no
table(is.na(dep_df$start_date)) #no
table(is.na(dep_df$end_date)) #no
table(is.na(dep_df$study_area)) #no

# Ensure location is a factor with desired ordering
dep_df <- dep_df %>%
  mutate(location = factor(location))

## faceted ggplot for camera activity by study area
facet_cam_activity <- ggplot(dep_df) +
  geom_segment( # add a horizontal segment for each location
    aes(
      x = start_date, # start_date for start of segment
      xend = end_date, #end_date for end
      y = location, #location for position on y-axis
      yend = location
    ),
    linewidth = 1
  ) +
  geom_point(aes(x = start_date, y = location), size = 2) + # add a point at start date
  geom_point(aes(x = end_date, y = location), size = 2) + # add a point at end date
  facet_wrap(~ study_area, scales = "free_y") +
  labs(
    title = "Camera Activity by Study Area",
    x = "Date",
    y = "Location"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 9)
  )

win.graph()
facet_cam_activity

## one TDN camera active until 2023, which one?
tdn_check <- dep_df %>% filter(study_area == "ThaideneNëné")
max(tdn_check$end_date)
tdn_check$location[tdn_check$end_date == "2023-07-10"] # BIO-TDN-29-02 - true, it was deployed until 2023

## Now I want to add the out of range dates to each location
## Filter for out of range images in img_df and sort them into distinct intervals per location
oor_intervals <- img_df %>%
  filter(image_fov == "OOR") %>% # filter for OOR
  filter(image_trigger_mode == "Time Lapse") %>% # also filter for timelapse, so there is only one OOR per day
  arrange(study_area, location, image_date_time) %>% ## select only relevant columns
  group_by(study_area, location) %>% 
  mutate(
    # TRUE when this row starts a new interval
    new_interval = image_date_time - lag(image_date_time) > 0, #new interval started when there is at least a day between OOR images
    new_interval = if_else(is.na(new_interval), TRUE, new_interval),
    
    # rleid assigns a unique ID to each run of TRUE/FALSE
    interval_id = rleid(new_interval)
  ) %>%
  group_by(study_area, location, interval_id) %>%
  summarise(
    interval_start = min(image_date_time),
    interval_end   = max(image_date_time),
    n_images       = n(),
    .groups = "drop"
  )

summary(oor_intervals)

## Convert to a df with one row per location and multiple OOR intervals (if applicable)
oor_wide <- oor_intervals %>%
  arrange(study_area, location, interval_start) %>%
  group_by(study_area, location) %>%
  mutate(interval_num = row_number()) %>% # number intervals per location
  ungroup() %>%
  pivot_wider(
    id_cols = c(study_area, location),
    names_from = interval_num, #generate interval number
    values_from = c(interval_start, interval_end),
    names_glue = "{.value}_{interval_num}" ## add interval number to interval_start and _end
  ) %>% 
  select(study_area, location, interval_start_1, interval_end_1, interval_start_2, interval_end_2, interval_start_3, interval_end_3) #reorder so start and end times are together



## Add oor_wide intervals to dep_df with left_join
dep_df <- dep_df %>% left_join(oor_wide, by = c("study_area", "location"))


## Still need to adjust days_active with OOR days, and update daily_lookup/row_lookup, camera activity plots, etc

