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
       "data.table")

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

getwd()
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

## Remove source_file column
dep_df <- dep_df %>%
  select(-source_file)

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


## Save deployment data across projects
write.csv(dep_df, "data/all_projects_deployment_data_20260312.csv")

## still need to add Out of range dates
