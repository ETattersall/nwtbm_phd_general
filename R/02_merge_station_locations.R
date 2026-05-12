###################
### 02_merge_station_locations.R
### Mapping station locations from datasets 
### included in fire-gamebird and fire-ungulate chapters
### Started 28 October 2024 by Erin Tattersall
### Updated with stations from wildtrax in May 2025
##################

#### Environment set up ####
## Load required packages (should already be installed)

x <- c("wildrtrax",
       "purrr",
       "tidyverse",
       "data.table",
       "stringr",
       "sf")



## install.packages(x) ## should already be installed ####
lapply(x, require, character.only = TRUE)


## Authenticate into WildTrax. Access local script for WT_USERNAME and WT_PASSWORD 
source("wildtrax_login.R") ## This will set the environment variables WTUSERNAME and WTPASSWORD
wt_auth()



#### Download Station locations from wildtrax - WT downloads not working (Apr 9 - 13 2026) ####
# cam_projects <- wt_get_projects(
#   sensor = "CAM")
# 
# glimpse(cam_projects)
# 
# 
# ## Filter to my target projects only, using project IDs: 712 (Thaidene Nene), 2183 (Fort Smith), 2102 (Norman Wells), 1906 (Sambaa K'e), 2935 (Gameti), 1465 (Edehzhie)
# cam_projects <- cam_projects %>% filter(project_id == "712" |
#                                           project_id == "2183" |
#                                           project_id == "2102" |
#                                           project_id == "1906" |
#                                           project_id == "2935" |
#                                           project_id == "1465")
# 
# ## Download camera location reports
# cam_locs_wt <- wt_download_report(project_id = cam_projects$project_id,
#                                       sensor_id = "CAM",
#                                      report = "main")

# WT downloads broken, but all camera location data already aggregated (and converted to shapefile)
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd_general/data/sensor_locations")
list.files()

cam_locs <- read.csv("all_projects_cam_locations_20260327.csv")

 
summary(cam_locs)
table(is.na(cam_locs$latitude)) # No NAs in latitude

table(cam_locs$study_area)
 

#### Merge ARU station locations #### 
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd_general/data/wt_aru_reports_allprojects")
list.files() # 9 files (2 TDN projects, 3 Gameti projects, 1 for all others)

## Read CSV files into R and bind them together
# List location reports in the directory
aru_loc_csv <- list.files(pattern = "\\location_report.csv$") ## object listing all project location reports (should be 9)

# Read and bind all CSVs, adding a column for the source file
aru_locs <- rbindlist(lapply(aru_loc_csv, function(file) {
  dt <- fread(file)
  dt[, source_file := basename(file)]
  return(dt)
}))

glimpse(aru_locs) # 887 locations
summary(aru_locs) ## No lat/long NAs
length(unique(aru_locs$location)) # 743 -some repeat locations

## Check source file names
table(aru_locs$source_file)


### Add a column for study area
aru_locs <- aru_locs %>%
  mutate(study_area = case_when(
    str_detect(source_file, "Edéhzhíe") ~ "Edéhzhíe",
    str_detect(source_file, "Fort_Smith") ~ "FortSmith",
    str_detect(source_file, "Gameti") ~ "Gameti",
    str_detect(source_file, "Norman_Wells") ~ "NormanWells",
    str_detect(source_file, "Sambaa_K'e") ~ "SambaaK'e",
    str_detect(source_file, "TDN") ~ "ThaideneNëné",
    str_detect(source_file, "Thaidene_Nëné") ~ "ThaideneNëné",
    TRUE ~ NA_character_  # Default case if no match
  ))

table(aru_locs$study_area)
table(is.na(aru_locs$study_area)) ## 0 NAs


## Remove source_file column
aru_locs <- aru_locs %>%
  select(-source_file)

glimpse(aru_locs)

## Will likely be some duplicates between Gameti and TDN files
aru_locs <- aru_locs[!duplicated(aru_locs), ] # Down from 886 to 743


## I know there are duplicate stations marked X in the aru TDN dataset. These are duplicate recordings uploaded to WildTrax and those stations can be removed
## Find rows where location includes _X
x_rows <- aru_locs %>%
  filter(str_detect(location, "_X")) ## 11 rows removed
## Remove these rows from aru_stns
aru_locs <- aru_locs %>%
  filter(!str_detect(location, "_X"))

table(aru_locs$study_area)

## Save the ARU station locations
write.csv(aru_locs, "C:/Users/tatterer.stu/Desktop/nwtbm_phd_general/data/sensor_locations/all_projects_aru_locations_20260413.csv", row.names = FALSE)

# reset wd
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd_general")

## Read back in ARU station locations (if env has been cleared)
#aru_locs <- read.csv("data/sensor_locations/all_projects_aru_locations_20260413.csv")

## Reorder both aru_locs and cam_locs first by study_area, then by location.
aru_locs <- aru_locs %>%
  arrange(study_area, location)


glimpse(aru_locs)

## How many NAs in aru study_area?
table(is.na(aru_locs$study_area)) ## 0 NAs in study_area


cam_locs <- cam_locs %>%
  arrange(study_area, location)

glimpse(cam_locs)

## How many NAs in cam study_area?
table(is.na(cam_locs$study_area)) ## 0 NAs in study_area

## Site already added to camera data, but not to ARU data. Since the names don't yet match, I won't keep that column yet (will add back later once naming aligns)
## Keep matching columns: study_area, location, latitude, longitude
aru_locs <- aru_locs %>% select(study_area, location, latitude, longitude)
cam_locs <- cam_locs %>% select(study_area, location, latitude, longitude)

## Add a column for sensor type
aru_locs$sensor_type <- "aru"

cam_locs$sensor_type <- "camera"

## rbind cam_locs and aru_locs
all_locs <- rbind.data.frame(aru_locs, cam_locs)



## How many unique stations are in all_locs?
length(unique(all_locs$location)) ## 1053 unique station locations.


## Merge all rows in all_locs that have the same study_area, location name, latitude, and longitude.
all_locs <- all_locs %>%
  group_by(study_area, location, latitude, longitude) %>%
  arrange(study_area,location, sensor_type) %>% 
  summarise(sensor_type = paste(unique(sensor_type), collapse = "_")) %>%  # Combine sensor types into a single string
  ungroup()


## Rearrange all_locs to order by study_area and location
all_locs <- all_locs %>%
  arrange(study_area, location)

summary(all_locs) ##1170 stations


table(all_locs$study_area) ## most study areas have duplicate locations (more listed than in aru_locs or cam_locs individually)
# Only Edehzhie and TDN merged (and TDN only partially). Other 4 did not merge at all


table(all_locs$sensor_type) ## Merged 268 stations with matching study_area, location, latitude, and longitude. I don't think that many stations were aru or camera only, indicating discrepancies

#### Fix naming discrepancies, one study area at a time. Save Gameti for last since Brad et al. were working on that one ####
## 1. Edehzhie
ede_all_locs <- all_locs %>% filter(study_area == "Edéhzhíe") %>%
  arrange(sensor_type, location) ## arrange stations by sensors, then by location

## How many locations are ARU, cameras, or both?
table(ede_all_locs$sensor_type)

## Naming is consistent across wt projects (all done by CWS) but not with Google Drive (wt doesn't have consistent digits in suffixes).
# Make suffixes consistent with Google Drive
ede_all_locs <- ede_all_locs %>%
  mutate(
    prefix = str_remove(location, "-\\d+-\\d+$"),
    n1 = str_extract(location, "(?<=-)\\d+(?=-\\d+$)"),
    n2 = str_extract(location, "(?<=-)\\d+$"),
    location_std = paste0(
      prefix, "-",
      sprintf("%02d", as.integer(n1)), "-",
      sprintf("%02d", as.integer(n2))
    )
  ) %>%
  select(-prefix, -n1, -n2)

## Save name look up table
ede_loc_names <- ede_all_locs %>% select(location, location_std)
# rename columns
colnames(ede_loc_names) <- c("location_wt", "location_std")
## Add study area to ede_loc_names
ede_loc_names$study_area <- rep("Edéhzhíe", nrow(ede_loc_names))

## Then rename location_std to location in ede_locs
ede_all_locs <- ede_all_locs %>% 
  select(-location) %>% 
  select(study_area, location_std, latitude, longitude, sensor_type)
colnames(ede_all_locs) <- c("study_area", "location", "latitude", "longitude", "sensor_type")

# Now re-arrange again (only by location) to compare sensor types to drive
ede_all_locs <- ede_all_locs %>% arrange(location)

## Re-name ede_all_locs to ede_locs (std naming with below)
ede_locs <- ede_all_locs
rm(ede_all_locs)

## Sensor types at each location do not match what is indicated in the google drive for what was deployed or uploaded. Noting issues in data/station_data_alignment_20260421.xlsx
## Also - cameras are not listed for ENWA sites 08,12,13,14,17, but the camera data had not been processed at time of download
## Convert these locations to aru_camera
ede_locs <- ede_locs %>%
  mutate(
    sensor_type = if_else(
      str_detect(location, "^([^\\-]+-){2}(08|12|13|14|17)-"),
      "aru_camera",
      sensor_type
    )
  )


## 2. Fort Smith
fs_all_locs <-   all_locs %>% filter(study_area == "FortSmith") %>%
  arrange(sensor_type, location) ## arrange stations by sensors, then by location

## Create naming table - fill wt locations and study area
fs_loc_names <- as.data.frame(fs_all_locs$location)
colnames(fs_loc_names) <- "location_wt"
fs_loc_names$location_std <- rep(NA, nrow(fs_loc_names))
fs_loc_names$study_area <- rep("FortSmith", nrow(fs_loc_names))



## Naming difference come from the inclusion/exclusion of the BMS prefix, or differences in digits in suffixes
## Add BMS and ensure that all numeric suffixes have 3 digits

fs_all_locs2 <- fs_all_locs %>%
  mutate(
    # Ensure prefix BMS-
    location = if_else(
      str_starts(location, "BMS-"),
      location,
      paste0("BMS-", location)
    ),
    
    # Pad numeric-only suffixes to 3 digits
    location = str_replace_all(
      location,
      "(?<=-)(\\d+)(?=-|$)",
      function(x) if (str_detect(x, "^[0-9]+$")) sprintf("%03d", as.integer(x)) else x
    ))

## Merge all rows in fs_all_locs that have the same study_area, location name, latitude, and longitude.
fs_all_locs2 <- fs_all_locs2 %>%
  group_by(study_area, location, latitude, longitude) %>%
  arrange(study_area,location, sensor_type) %>% 
  summarise(sensor_type = paste(unique(sensor_type), collapse = "_")) %>%  # Combine sensor types into a single string
  ungroup()


## Sites deployed by FSMC (308,310,316) now have 5 parts in the ARU data. Drop the last part  
# Identify FortSmith rows with exactly 4 hyphens (i.e., 5 parts)
fsmc_sites <- stringr::str_count(fs_all_locs2$location, "-") == 4

# Drop the last hyphen-separated part ONLY for those rows
fs_all_locs2$location[fsmc_sites] <-
  sub("-[^-]+$", "", fs_all_locs2$location[fsmc_sites])

## Fill naming table - repeat above for fs_loc_names$location_std
## Add BMS and ensure that all numeric suffixes have 3 digits

fs_loc_names <- fs_loc_names %>%
  mutate(
    # Ensure prefix BMS-
    location_std = if_else(
      str_starts(location_wt, "BMS-"),
      location_wt,
      paste0("BMS-", location_wt)
    ),
    
    # Pad numeric-only suffixes to 3 digits
    location_std = str_replace_all(
      location_std,
      "(?<=-)(\\d+)(?=-|$)",
      function(x) if (str_detect(x, "^[0-9]+$")) sprintf("%03d", as.integer(x)) else x
    ))

## Drop 5th part of FSMC sites
fsmc_site_names <- stringr::str_count(fs_loc_names$location_std, "-") == 4

# Drop the last hyphen-separated part ONLY for those rows
fs_loc_names$location_std[fsmc_site_names] <-
  sub("-[^-]+$", "", fs_loc_names$location_std[fsmc_site_names])

### Coordinate discrepancies
## Are there still duplicate locations? This would indicate coordinate issues
table(duplicated(fs_all_locs2$location)) ## 13 duplicated

## Isolate the stations that are duplicated (both camera and aru row)
dup_fs <- fs_all_locs2 %>% 
  group_by(location) %>% 
  filter(n() > 1) # Keep only rows with more than one occurrence of the same location

## Comparing to master files on google drive shows that camera coordinates are correct. Some ARU coords are the originals, not the deployed
## BMS-TLU-194-027 is slightly off for both, but camera is closer: 60.7432, -110.5469
## Fix the ARU coordinates to match the camera coordinates
dup_fs <- dup_fs %>%
  group_by(location) %>%
  mutate(
    latitude = if_else(
      sensor_type == "aru",
      latitude[sensor_type == "camera"][1],
      latitude
    ),
    longitude = if_else(
      sensor_type == "aru",
      longitude[sensor_type == "camera"][1],
      longitude
    )
  ) %>%
  ungroup()

## Revise BMS-TLU-194-027

dup_fs <- dup_fs %>%
  mutate(
    latitude  = if_else(location == "BMS-TLU-194-027", 60.7432, latitude),
    longitude = if_else(location == "BMS-TLU-194-027", -110.5469, longitude)
  )

## Correct coordinates in fs_all_locs
fs_all_locs3 <- fs_all_locs2 %>%
  left_join(
    dup_fs %>%
      select(location, sensor_type,
             corrected_lat = latitude,
             corrected_lon = longitude),
    by = c("location", "sensor_type")
  ) %>%
  mutate(
    latitude  = if_else(!is.na(corrected_lat), corrected_lat, latitude),
    longitude = if_else(!is.na(corrected_lon), corrected_lon, longitude)
  ) %>%
  select(-corrected_lat, -corrected_lon)

## Merge again
fs_all_locs3 <- fs_all_locs3 %>%
  group_by(study_area, location, latitude, longitude) %>%
  arrange(study_area,location, sensor_type) %>% 
  summarise(sensor_type = paste(unique(sensor_type), collapse = "_")) %>%  # Combine sensor types into a single string
  ungroup()

## Results in 60 stations (correct) - should be 6 camera only stations
table(fs_all_locs3$sensor_type)

## rename fs_all_locs3 to fs_locs, remove other temporary dfs EXCEPT fs_loc_names
fs_locs <- fs_all_locs3
rm(fs_all_locs, fs_all_locs2, fs_all_locs3, dup_fs)

##### 3. Norman Wells
nw_all_locs <-   all_locs %>% filter(study_area == "NormanWells") %>%
  arrange(sensor_type, location) ## arrange stations by sensors, then by location

## There are a few different naming conventions, so go through them one at a time

## locations beginning BMS-NWS should follow the format 'BMS-NWS-01-1234-01' (numeric suffixes should have 2,4, and 2 digits respectively)
df_nws <- nw_all_locs %>%
  filter(str_starts(location, "NWS")) %>%
  separate(
    location,
    into = c("p1", "n1", "n2", "n3"),
    sep = "-",
    remove = FALSE
  ) %>%
  mutate(
    n1 = sprintf("%02d", as.integer(n1)),
    n2 = sprintf("%04d", as.integer(n2)),
    n3 = sprintf("%02d", as.integer(n3)),
    location_std = paste(p1, n1, n2, n3, sep = "-")
  ) %>%
  select(location, location_std)


## all other locations should have 3 digits in the 3rd section and 2 digits in the fourth section (but keep letters as they are)
df_nw_other <- nw_all_locs %>%
  filter(!str_starts(location, "NWS")) %>%
  separate(
    location,
    into = c("p1", "p2", "n1", "n2"),
    sep = "-",
    remove = FALSE,
    extra = "merge"
  ) %>%
  mutate(
    n1 = sprintf("%03d", as.integer(n1)),
    n2 = if_else(
      str_detect(n2, "^[0-9]+$"),
      sprintf("%02d", as.integer(n2)),
      n2
    ),
    location_std = paste(p1, p2, n1, n2, sep = "-")
  ) %>%
  select(location, location_std)


## Left join new location names to nw_all_locs
nw_all_locs2 <- 
  nw_all_locs %>%
  left_join(
    bind_rows(df_nws, df_nw_other),
    by = "location"
  ) %>%
  mutate(location = coalesce(location_std, location)) %>%
  select(-location_std)

## Now merge sensor types
nw_all_locs2 <- nw_all_locs2 %>%
  group_by(study_area, location, latitude, longitude) %>%
  arrange(study_area,location, sensor_type) %>% 
  summarise(sensor_type = paste(unique(sensor_type), collapse = "_")) %>%  # Combine sensor types into a single string
  ungroup()

table(nw_all_locs2$sensor_type) ## only 2 sites merged, but there were 80 cameras deployed and 102 ARUs, so expect at least 102 stations

# Visual inspection shows that some sites with same location, lat, long are not merging
# Check if lat/long differ slightly by counting rows in each group and filtering for duplicates

nw_all_locs2 %>%
  count(study_area, location, latitude, longitude) %>%
  filter(n > 1)
# 0 returns - so lat/longs might be slightly different

## Confirm precision issues - how many distinct lats and longs exist for each location?
nw_all_locs2 %>%
  group_by(study_area, location) %>%
  summarise(
    lat_vals = n_distinct(latitude),
    lon_vals = n_distinct(longitude),
    .groups = "drop"
  ) %>%
  filter(lat_vals > 1 | lon_vals > 1)
# 2 lats and/or longs for 70 locations

## Might need to just use coordinates from google drive. Save nw_all_locs2 and cross-check with deployment docs
write.csv(nw_all_locs2, "data/normanwells_coordinate_discrepancies.csv")

## Manual cross-checking showed that ARUs used the intended coordinates, not the deployed. Fixed manually, read back in csv
nw_locs_fixed <- read.csv("data/normanwells_coordinate_discrepancies.csv")
glimpse(nw_locs_fixed)

## replace latitude/longitude with fieldsheet_lat and fieldsheet_long
nw_locs <- nw_locs_fixed %>% select(study_area, location, fieldsheet_lat, fieldsheet_long, sensor_type, corrected_name)

## fix incorrect name (BMS-PRP-050-12 to BMS-NRA-050-12) and remove corrected_name
nw_locs <- nw_locs %>% 
  mutate(location = if_else(location == "BMS-PRP-050-12", corrected_name, location)) %>% 
  select(-corrected_name)
# rename columns
colnames(nw_locs) <- c("study_area", "location", "latitude", "longitude", "sensor_type")

## Now merge sensor_types
nw_locs <- nw_locs %>% 
group_by(study_area, location, latitude, longitude) %>%
  arrange(study_area,location, sensor_type) %>% 
  summarise(sensor_type = paste(unique(sensor_type), collapse = "_")) %>%  # Combine sensor types into a single string
  ungroup()

## Check summary of sensors
table(nw_locs$sensor_type)

## Check number of unique locations
length(unique(nw_locs$location)) #109 - no duplicates

## Need name lookup table - combine df_nws and df_other
nw_loc_names <- bind_rows(df_nws, df_nw_other)

## Note corrected name BMS-NRA-050-12 as standardized location for BMS-PRP-50-12
nw_loc_names <- nw_loc_names %>% 
  mutate(location_std = if_else(location == "BMS-PRP-50-12", "BMS-NRA-050-12", location_std))

## Rename columns
colnames(nw_loc_names) <- c("location_wt", "location_std")
## Add study area
nw_loc_names$study_area <- rep("NormanWells", nrow(nw_loc_names))

## Clean up temporary NW dfs (keep only nw_locs and nw_loc_names)
rm(df_nw_other, df_nws, nw_all_locs, nw_all_locs2, nw_locs_fixed)

##### 4. Sambaa K'e
sk_all_locs <- all_locs %>% filter(study_area == "SambaaK'e") %>%
  arrange(location, sensor_type) ## arrange stations by location, then sensor type

table(sk_all_locs$sensor_type)


## Difference in names between cameras and ARUs is from difference in digits in the last suffix - standardize at 3
df_sk <- sk_all_locs %>%
  separate(
    location,
    into = c("p1", "n1", "s1", "n2"),
    sep = "-",
    remove = FALSE
  ) %>%
  mutate(
    n2 = sprintf("%03d", as.integer(n2)),
    location_std = paste(p1, n1, s1, n2, sep = "-")
  ) %>%
  select(location, location_std)

## Create naming table, rename columns and add study area
sk_loc_names <- df_sk
colnames(sk_loc_names) <- c("location_wt", "location_std")
sk_loc_names$study_area <- rep("SambaaK'e", nrow(sk_loc_names))

## Remove duplicate rows if they exist (e.g. locations with cam and ARU that already matched)
df_sk <- df_sk[!duplicated(df_sk), ]

## Left join new location names to sk_all_locs
sk_all_locs2 <- 
  sk_all_locs %>%
  left_join(
    df_sk,
    by = "location"
  ) %>%
  mutate(location = coalesce(location_std, location)) %>%
  select(-location_std)

## Now merge ARU and camera stations
sk_all_locs2 <- sk_all_locs2 %>%
  group_by(study_area, location, latitude, longitude) %>%
  arrange(study_area,location, sensor_type) %>% 
  summarise(sensor_type = paste(unique(sensor_type), collapse = "_")) %>%  # Combine sensor types into a single string
  ungroup()

## None merged - are there duplicates?
dup_sk <- sk_all_locs2 %>% 
  group_by(location) %>% 
  filter(n() > 1) # Keep only rows with more than one occurrence of the same location

## Only 22 duplicates - but all cameras were deployed with an ARU. Suggests that not all those ARUs were uploaded

# Visual inspection suggests many of these coordinates also have precision drift, and that it affects all stations (not just those that I'm trying to merge)
# also not all the locations in the google drive station summary are represented in WildTrax data (with no record of what was uploaded for ARUs)
## Save for manual check against Google Drive
write.csv(sk_all_locs2, "data/sambaake_coordinate_discrepancies.csv")

## Sambaa K'e coordinates fixed manually (ARU coords were originals, not deployed. Also some differences between E and W prefixes, but not between camera/ARU, so ignore)
sk_locs_fixed <- read.csv("data/sambaake_coordinate_discrepancies.csv")
glimpse(sk_locs_fixed)
summary(sk_locs_fixed$corrected_name) ## all NAs - no names fixed manually

## replace latitude/longitude with fieldsheet_lat and fieldsheet_long
sk_locs <- sk_locs_fixed %>% select(study_area, location, fieldsheet_lat, fieldsheet_long, sensor_type)

# rename columns
colnames(sk_locs) <- c("study_area", "location", "latitude", "longitude", "sensor_type")

## Now merge sensor_types
sk_locs <- sk_locs %>% 
  group_by(study_area, location, latitude, longitude) %>%
  arrange(study_area,location, sensor_type) %>% 
  summarise(sensor_type = paste(unique(sensor_type), collapse = "_")) %>%  # Combine sensor types into a single string
  ungroup()

## Check summary of sensors
table(sk_locs$sensor_type) ## 11 stations with both sensors

## Check number of unique locations
length(unique(sk_locs$location)) #88 - no duplicates

## One additional error found when plotting points later - latitude for WR-3-W-041 should be 61.00307
sk_locs <- sk_locs %>% 
  mutate(latitude = ifelse(location == "WR-3-W-041", 61.00307, latitude))


## Clean up all files except sk_locs and sk_loc_names
rm(df_sk,dup_sk, sk_all_locs, sk_all_locs2, sk_locs_fixed)

#### 5. Thaidene Nene ####
tdn_all_locs <- all_locs %>% filter(study_area == "ThaideneNëné") %>%
  arrange(location, sensor_type) ## arrange stations by location, then sensor type

table(tdn_all_locs$sensor_type)

## Create naming table - fill wt locations and study area
tdn_loc_names <- as.data.frame(tdn_all_locs$location)
colnames(tdn_loc_names) <- "location_wt"
tdn_loc_names$location_std <- rep(NA, nrow(tdn_loc_names))
tdn_loc_names$study_area <- rep("ThaideneNëné", nrow(tdn_loc_names))

## All of the BIO-TDN locations have the same names (so already merged), except for 027-07NR. Fix that one
tdn_all_locs <- tdn_all_locs %>%
  mutate(
    location = if_else(location == "BIO-TDN-021-7NR", "BIO-TDN-021-07NR", location))

## The BMS stations have the same digit differences as other projects
df_tdn_bms <- tdn_all_locs %>%
  filter(str_starts(location, "BMS")) %>%
  separate(
    location,
    into = c("p1", "p2", "n1", "n2"),
    sep = "-",
    remove = FALSE,
    extra = "merge"
  ) %>%
  mutate(
    n1 = sprintf("%03d", as.integer(n1)),
    n2 = if_else(
      str_detect(n2, "^[0-9]+$"), # leave suffixes with letters alone
      sprintf("%02d", as.integer(n2)),
      n2
    ),
    location_std = paste(p1, p2, n1, n2, sep = "-")
  ) %>%
  select(location, location_std)


### naming table - add all tdn names to table in both location columns
tdn_loc_names$location_wt <- tdn_all_locs$location
tdn_loc_names$location_std <- tdn_all_locs$location

## Fix 027-07NR
tdn_loc_names <- tdn_loc_names %>%
  mutate(
    location_std = if_else(location_std == "BIO-TDN-021-7NR", "BIO-TDN-021-07NR", location_std))

### Fix digits in BMS stations (using df_tdn_bms)
tdn_loc_names <- tdn_loc_names %>%
  left_join(
    df_tdn_bms %>%
      select(location, location_std) %>%
      rename(location_wt = location, #rename location to match naming table
             new_location_std = location_std), # add new column for the BMS std names
    by = "location_wt" ## add df_tdn_bms to tdn_loc_names by matching location_wt
  ) %>%
  mutate(
    location_std = coalesce(new_location_std, location_std) #combine bms std names with other standard names
  ) %>%
  select(-new_location_std) #remove new bms std names column


## Merging stations of the same name - Left join new location names to tdn_all_locs
tdn_all_locs2 <- 
  tdn_all_locs %>%
  left_join(
    df_tdn_bms,
    by = "location"
  ) %>%
  mutate(location = coalesce(location_std, location)) %>%
  select(-location_std)

## Now merge ARU and camera stations
tdn_all_locs2 <- tdn_all_locs2 %>%
  group_by(study_area, location, latitude, longitude) %>%
  arrange(study_area,location, sensor_type) %>% 
  summarise(sensor_type = paste(unique(sensor_type), collapse = "_")) %>%  # Combine sensor types into a single string
  ungroup()

## Still a bunch of duplicates though - so there are coordinate issues, likely same as others. Check manually
write.csv(tdn_all_locs2, "data/tdn_coordinate_discrepancies_renamed.csv")
## Manual check confirms that ARU coordinates were original points, camera coordinates were deployment coords
## Also no ARUs deployed alone, so no need to find those deployment coords

## Convert ARU coordinates to camera coordinates for duplicated rows
tdn_all_locs2 <- tdn_all_locs2 %>%
  group_by(location) %>%
  mutate(
    latitude = if_else(
      sensor_type == "aru" &
        any(sensor_type == "camera"),
      latitude[sensor_type == "camera"][1],
      latitude
    ),
    longitude = if_else(
      sensor_type == "aru" &
        any(sensor_type == "camera"),
      longitude[sensor_type == "camera"][1],
      longitude
    )
  ) %>%
  ungroup()

## Now merge sensor_types
tdn_locs <- tdn_all_locs2 %>%
  group_by(study_area, location, latitude, longitude) %>%
  arrange(study_area,location, sensor_type) %>% 
  summarise(sensor_type = paste(unique(sensor_type), collapse = "_")) %>%  # Combine sensor types into a single string
  ungroup()

## Check summary of sensors
table(tdn_locs$sensor_type) ## 274 aru_camera, 33 camera

## Check number of unique locations
length(unique(tdn_locs$location)) #307 - no duplicates

## Clean up temporary dfs
rm(df_tdn_bms, tdn_all_locs, tdn_all_locs2)

#### 6. Gameti ####
## Liam should have fixed Gameti discrepancies on WildTrax. But this will likely only fix the camera data coordinate issues
## Download Gameti camera and ARU data from WildTrax (manually, since wildrtrax download function is still being weird)
## Load in Gameti camera location reports
list.files("data/wt_camera_reports_allprojects")
gam_cam_locs <- read.csv("data/wt_camera_reports_allprojects/NWTBM_Gameti_2023-2024_location_report.csv")

## Load in all 3 Gameti ARU location reports
gam_aru_csv <- list.files("data/wt_aru_reports_allprojects/Gameti_locations")

# Set wd to file folder
setwd("data/wt_aru_reports_allprojects/Gameti_locations")

# Read and bind all CSVs, adding a column for the source file
gam_aru_locs <- rbindlist(lapply(gam_aru_csv, function(file) {
  dt <- fread(file)
  dt[, source_file := basename(file)]
  return(dt)
}))

## Reset wd
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd_general")

## Check source file names
table(gam_aru_locs$source_file)


### Add a column for study area
gam_aru_locs <- gam_aru_locs %>%
  mutate(study_area = case_when(
    str_detect(source_file, "Gameti") ~ "Gameti",
    TRUE ~ NA_character_  # Default case if no match
  ))

table(gam_aru_locs$study_area)
table(is.na(gam_aru_locs$study_area)) ## 0 NAs


## Remove source_file column
gam_aru_locs <- gam_aru_locs %>%
  select(-source_file)

glimpse(gam_aru_locs)

## Remove duplicate locations
gam_aru_locs <- gam_aru_locs[!duplicated(gam_aru_locs), ] # Down from 220 to 76


glimpse(gam_cam_locs)
## add study_area column
gam_cam_locs$study_area <- rep("Gameti", nrow(gam_cam_locs))


## Keep matching columns: study_area, location, latitude, longitude
gam_aru_locs <- gam_aru_locs %>% select(study_area, location, latitude, longitude)
gam_cam_locs <- gam_cam_locs %>% select(study_area, location, latitude, longitude)

## Add a column for sensor type
gam_aru_locs$sensor_type <- "aru"

gam_cam_locs$sensor_type <- "camera"

## rbind cam_locs and aru_locs
gam_locs <- rbind.data.frame(gam_aru_locs, gam_cam_locs)


## How many unique stations are in gam_locs?
length(unique(gam_locs$location)) ## 142 unique station locations.


## Merge all rows in gam_locs that have the same study_area, location name, latitude, and longitude.
gam_locs <- gam_locs %>%
  group_by(study_area, location, latitude, longitude) %>%
  arrange(study_area,location, sensor_type) %>% 
  summarise(sensor_type = paste(unique(sensor_type), collapse = "_")) %>%  # Combine sensor types into a single string
  ungroup()
## none merged - fix names

## all already have BMS prefix - standardize digits in prefixes (use this as naming table too)
gam_loc_names <- gam_locs %>%
  separate(
    location,
    into = c("p1", "p2", "n1", "n2"),
    sep = "-",
    remove = FALSE
  ) %>%
  mutate(
    n1 = sprintf("%03d", as.integer(n1)),
    n2 = if_else(
      str_detect(n2, "^[0-9]+$"),
      sprintf("%02d", as.integer(n2)), #keep letters as is
      n2
    ),
    location_std = paste(p1, p2, n1, n2, sep = "-")
  ) %>%
  select(location, location_std)


## Add std names to gam_locs
gam_locs <- 
  gam_locs %>%
  left_join(
    gam_loc_names,
    by = "location"
  ) %>%
  mutate(location = coalesce(location_std, location)) %>%
  select(-location_std)

# Rename location column in gam_loc_names
colnames(gam_loc_names) <- c("location_wt", "location_std")
## add study_area
gam_loc_names$study_area <- rep("Gameti", nrow(gam_loc_names))

## Try merging gam_locs again 
gam_locs <- gam_locs %>%
  group_by(study_area, location, latitude, longitude) %>%
  arrange(study_area,location, sensor_type) %>% 
  summarise(sensor_type = paste(unique(sensor_type), collapse = "_")) %>%  # Combine sensor types into a single string
  ungroup()
## 8 merged

## Initial comparison shows mix up issues haven't yet been fixed in wt. ARU coords seem to mostly be correct. Manually check.
write.csv(gam_locs, "data/gameti_coordinate_discrepancies.csv")

# Found one incorrect name: BMS-CRU-098-18 should be BMS-CRU-098-19 (camera). Correct in both gam_locs and gam_locs_names
gam_locs <- gam_locs %>% 
  mutate(location = ifelse(location == "BMS-CRU-098-18", "BMS-CRU-098-19", location))
gam_loc_names <- gam_loc_names %>% 
  mutate(location_std = ifelse(location_wt == "BMS-CRU-098-18", "BMS-CRU-098-19", location_std))

## Convert all camera coordinates to ARU coordinates
## Convert ARU coordinates to camera coordinates for duplicated rows
gam_locs <- gam_locs %>%
  group_by(location) %>%
  mutate(
    latitude = if_else(
      sensor_type == "camera" &
        any(sensor_type == "aru"),
      latitude[sensor_type == "aru"][1],
      latitude
    ),
    longitude = if_else(
      sensor_type == "camera" &
        any(sensor_type == "aru"),
      longitude[sensor_type == "aru"][1],
      longitude
    )
  ) %>%
  ungroup()

## Try merging gam_locs again 
gam_locs <- gam_locs %>%
  group_by(study_area, location, latitude, longitude) %>%
  arrange(study_area,location, sensor_type) %>% 
  summarise(sensor_type = paste(unique(sensor_type), collapse = "_")) %>%  # Combine sensor types into a single string
  ungroup()

length(unique(gam_locs$location)) ## 79 - no duplicates

table(gam_locs$sensor_type) ## 3 camera only sites - manually check their coordinates
## BMS-CRU-176-01 and BMS-KLP-049-01 need to be corrected. BMS-KLP-047-01 is correct

gam_locs <- gam_locs %>% 
  mutate(latitude = ifelse(
    location == "BMS-CRU-176-01", 64.16051, 
    ifelse(location == "BMS-KLP-049-01", 64.04138, latitude))) %>% 
  mutate(longitude = ifelse(
    location == "BMS-CRU-176-01", -117.2560, 
    ifelse(location == "BMS-KLP-049-01", -117.4279, longitude)))

#####################
## Combine all station locations and name lookup tables

## Name lookups first
stn_name_lookup <- bind_rows(ede_loc_names, 
                             fs_loc_names, 
                             nw_loc_names, 
                             sk_loc_names, 
                             tdn_loc_names, 
                             gam_loc_names)

## Save
write.csv(stn_name_lookup, "data/nwtbm_station_name_lookup_table.csv")

## station location coordinates
nwtbm_stns <- bind_rows(ede_locs, 
                        fs_locs, 
                        nw_locs, 
                        sk_locs, 
                        tdn_locs, 
                        gam_locs)
## 822 locations - what's sensor break down?
table(nwtbm_stns$sensor_type) # total of 732 ARUs, 730 cameras, which is correct. 640 paired aru_cameras

## Add a site column - remove last suffix from location name 
## applies to all study areas except Sambaa K'e (not clustered, so leave name as is)
## Now add site column, where sites = everything but last suffix of location names
nwtbm_stns <- nwtbm_stns %>% 
  mutate(site = if_else(
    study_area == "SambaaK'e", location, # if study_area is Sambaa K'e, return location ID, otherwise
    gsub(pattern = "-(?:[^-]+)(?=\\s*,|$)", # locate the last hyphen-separated part of each location ID
         "", # remove the last hyphen-separated part (i.e., replace it with nothing)
         location, # column to look for matches
         perl = TRUE) ## uses perl-compatible regex strings
  ))

## Convert nwtbm_stns to spatial file
nwtbm_stns_sf <- st_as_sf(nwtbm_stns, coords = c("longitude", "latitude"), crs = 4326)
plot(nwtbm_stns_sf["study_area"]) # plot stn locations colored by study area

sk_sf <- nwtbm_stns_sf %>% filter(study_area == "SambaaK'e")
plot(sk_sf) ## plotting error fixed

## Save as geopackage
st_write(nwtbm_stns_sf, "data/sensor_locations/nwtbm_allsensor_locations_20260506.gpkg", delete_layer = TRUE)
## save as csv
write.csv(nwtbm_stns, "data/sensor_locations/nwtbm_allsensor_locations_20260506.csv")


### Save camera and aru locations as separate files
cam_stns <- nwtbm_stns %>% filter(sensor_type != "aru") # remove aru only stations
aru_stns <- nwtbm_stns %>% filter(sensor_type != "camera") # remove camera only stations

## Save csvs
write.csv(cam_stns, "data/sensor_locations/nwtbm_cam_locations_20260506.csv")
write.csv(aru_stns, "data/sensor_locations/nwtbm_aru_locations_20260506.csv")

### Convert cam and aru stns to spatial and save as geopackages
cam_stns_sf <- st_as_sf(cam_stns, coords = c("longitude", "latitude"), crs = 4326)
aru_stns_sf <- st_as_sf(aru_stns, coords = c("longitude", "latitude"), crs = 4326)

st_write(cam_stns_sf, "data/sensor_locations/nwtbm_cam_locations_20260506.gpkg", delete_layer = TRUE)
st_write(aru_stns_sf, "data/sensor_locations/nwtbm_aru_locations_20260506.gpkg", delete_layer = TRUE)
