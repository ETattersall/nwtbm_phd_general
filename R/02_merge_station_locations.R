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
       "data.table")

getwd()

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


## Clean up all files except sk_locs and sk_loc_names
rm(df_sk,dup_sk, sk_all_locs, sk_all_locs2, sk_locs_fixed)



## 5. Thaidene Nene
tdn_all_locs <- all_locs %>% filter(study_area == "ThaideneNëné") %>%
  arrange(location, sensor_type) ## arrange stations by location, then sensor type

table(tdn_all_locs$sensor_type)

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


## Left join new location names to nw_all_locs
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

## Still a bunch of duplicates though - so there are coordinate issues
dup_tdn <- tdn_all_locs2 %>% 
  group_by(location) %>% 
  filter(n() > 1) # Keep only rows with more than one occurrence of the same location
## 244 rows - 122 stations with coordinate discrepancies


##### Coordinate discrepancies - note: fix naming discrepancies FIRST ####
## Compare lat/long values for rows that have the same location
duplicated_merged_locs <- all_locs %>%
  group_by(location) %>%
  filter(n() > 1) # Keep only rows with more than one occurrence of the same location

table(duplicated_merged_locs$sensor_type)

## Rearrange duplicated_merged_stns df to have coord_aru and coord_cam columns and merge them into a single coord_lookup table
coord_lookup <- duplicated_merged_locs %>%
  mutate(lat_long = paste(latitude, longitude, sep = "_")) %>% # Create a column combining latitude and longitude
  mutate(coord_aru = ifelse(sensor_type == "aru", lat_long, NA),
         coord_cam = ifelse(sensor_type == "camera", lat_long, NA)) %>%
  select(study_area, location, coord_aru, coord_cam) %>%
  group_by(study_area, location) %>%
  summarise(coord_aru = paste(na.omit(coord_aru), collapse = ", "),
            coord_cam = paste(na.omit(coord_cam), collapse = ", ")) %>%
  ungroup()

glimpse(coord_lookup)
table(is.na(coord_lookup))



## 116 locations with discrepancies in coordinates
## Save this as a CSV to notify Liam/Eamon
write.csv(coord_lookup, "C:/Users/tatterer.stu/Desktop/nwtbm_phd_general/data/sensor_locations/NWTBM_station_coordinate_discrepancies_20260413.csv", row.names = FALSE)


## Which rows have identical latitude AND longitude but different location names?
## Create a column combining latitude and longitude in aru and cam dfs to join them
cam_locs <- cam_locs %>%
  mutate(lat_long = paste(latitude, longitude, sep = "_"))

aru_locs <- aru_locs %>%
  mutate(lat_long = paste(latitude, longitude, sep = "_"))

## Join by lat_long to see naming discrepancies. Inner join will only keep rows that are present in cam and aru)
bad_name <- inner_join(cam_locs, aru_locs, by = "lat_long")

## Remove unnecessary duplicate columns (lat, long, second study area, sensor type) - remember x is camera, y is aru
bad_name <- bad_name %>%
  select(study_area.x, lat_long, location.x, location.y)
#Rename
colnames(bad_name) <- c("study_area", "lat_long", "cam_name", "aru_name")

## Filter for rows where cam_name doesn't match aru_name
bad_name <- bad_name %>% 
  filter(cam_name != aru_name)



### Deprecated by bad_name table, which will need to be addressed manually

# ## How many lat_long combinations are unique?
# length(unique(all_locs$lat_long)) ##1106 
# 
# ## Find rows with the same lat_long
# dup_lat_long <- all_locs %>%
#   group_by(lat_long) %>%
#   filter(n() > 1) %>% # Keep only rows with more than one occurrence of the same lat_long
#   arrange(lat_long)

######## Re-run up to here (13 Apr 2026) - Apr 21: started fixing naming discrepancies for each study area above

## Most of these discrepancies were already identified except for a few. Also found two Norman Wells camera stations that have the same coordinates: BMS-NRA-050-18 and BMS-NRA-050-16
## Inspecting google drive dataset indicates coordinates for 16 were put in twice
## Coordinates for BMS-NRA-050-16 are: 65.34962, -126.53659 in google drive.
## Coordinates for BMS-NRA-050-18 are: 65.35197, -126.5247 in google drive.

## Find BMS-NRA-050-16 and 18 in aru_stns (do not have 0 in front)
# aru_stns %>%
#   filter(location %in% c("BMS-NRA-50-16", "BMS-NRA-50-18")) ## Coordinates in ARU dataset are same as google drive
# 
# ## Fix coordinates for BMS-NRA-050-18 in all_stns 
# all_stns[all_stns$location == "BMS-NRA-050-18", "latitude"] <- 65.35197
# all_stns[all_stns$location == "BMS-NRA-050-18", "longitude"] <- -126.5247
# 
# ## Re-run Lines 297 - 347 to check for fix


## Rearrange dup_lat_long df to have location_aru and location_cam columns and merge them into a single location_lookup table
location_lookup <- dup_lat_long %>%
  mutate(location_aru = ifelse(sensor_type == "aru", location, NA),
         location_cam = ifelse(sensor_type == "camera", location, NA)) %>%
  select(study_area, lat_long, location_aru, location_cam) %>%
  group_by(study_area, lat_long) %>%
  summarise(location_aru = paste(na.omit(location_aru), collapse = ", "),
            location_cam = paste(na.omit(location_cam), collapse = ", ")) %>%
  ungroup()

## Add column for proposed_standard_location. 
location_lookup$proposed_name <- NA


## Select name manually, because some are from aru and some from cam
## Need to first understand what conventions are being followed by CWS and NWTBM
## Rows : location_aru
# location_lookup$proposed_name[1:19] <- location_lookup$location_aru[1:19] #some of these still don't follow a standard convention
# ## Rows 20-49: location_cam
# location_lookup$proposed_name[20:49] <- location_lookup$location_cam[20:49]



## Save the location lookup table as well (similar to previous version but includes more TDN stations)
write.csv(location_lookup, "C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/NWTBM_point_locations/NWTBM_location_name_discrepancies_July162025.csv", row.names = FALSE)

## Based on manual inspection, I suspect that some of the Norman Wells Stations have slightly different coordinates AND names...
## Script to standardize location names by adding leading 0s
## Note that this does NOT add the letter prefixes

# Function to standardize the 'location' format
# This function splits the location string by hyphens,
# pads numeric segments with leading zeros to 3 digits,
# and then recombines the parts.
standardize_location <- function(location) {
  # Split the location string into parts
  parts <- unlist(strsplit(location, "-"))
  
  # Apply zero-padding to numeric parts
  standardized_parts <- sapply(parts, function(part) {
    if (grepl("^\\d+$", part)) {
      # Pad numeric part to 3 digits
      str_pad(part, width = 3, pad = "0")
    } else {
      # Leave non-numeric parts unchanged
      part
    }
  })
  
  # Recombine the parts into a single string
  paste(standardized_parts, collapse = "-")
}

## Apply the standardization function to the 'location' column in copy of all_stns
all_stns2 <- all_stns

glimpse(all_stns2)

# Apply the standardization function to the 'location' column of all_stns2
all_stns2$location <- sapply(all_stns2$location, standardize_location)

## Any missing locations?
table(is.na(all_stns2$location)) ## 0 NAs in location

## Rearrange all_stns to order by study_area and location
all_stns2 <- all_stns2 %>%
  arrange(study_area, location)


## How many unique stations are in all_stns2 now?
length(unique(all_stns2$location)) ## 940 unique station locations - so fixed 113 stations

## Fix BIO-TDN-021-7NR to BIO-TDN-021-07NR
all_stns2$location[all_stns2$location == "BIO-TDN-021-7NR"] <- "BIO-TDN-021-07NR"

## Other naming discrepancies seem to be because some camera stations in Gameti and NormanWells do not include the prefix BMS but ARU locations do

# Function to add 'BMS-' prefix if location has only 3 parts
add_bms_prefix <- function(location) {
  # Count the number of hyphens
  num_parts <- str_count(location, "-") + 1
  
  # If there are only 3 parts, add the prefix
  if (num_parts == 3) {
    return(paste0("BMS-", location))
  } else {
    return(location)
  }
}

# Apply the function to the 'location' column
all_stns2$location <- sapply(all_stns2$location, add_bms_prefix)

## How many unique stations are in all_stns2 now?
length(unique(all_stns2$location)) #823 ## 117 stations were fixed by adding the BMS prefix

glimpse(all_stns2)

## Re-merge all rows in all_stns2 that have the same study_area, location name, latitude, and longitude.
all_stns_merged2 <- all_stns2 %>%
  group_by(study_area, location, latitude, longitude) %>%
  arrange(study_area,location, sensor_type) %>% 
  summarise(sensor_type = paste(unique(sensor_type), collapse = "_"), # Combine sensor types into a single string
            organization = first(organization), # Keep the first organization name
            location_id = first(location_id), # Keep the first location_id
            location_buffer_m = first(location_buffer_m), # Keep the first buffer size
            elevation = first(elevation), # Keep the first elevation value
            location_visibility = first(location_visibility), # Keep the first visibility value
            location_comments = first(location_comments)) %>% # Keep the first comments
  ungroup()


summary(all_stns_merged2) ##1113 stations
table(all_stns_merged2$study_area)
# How many NAs occur in study_area?
table(is.na(all_stns_merged2$study_area)) ## 0 NAs in study_area
table(all_stns_merged2$sensor_type) # 324 aru_camera - still a lot of single sensor stations

## Create a column combining latitude and longitude
all_stns_merged2 <- all_stns_merged2 %>%
  mutate(lat_long = paste(latitude, longitude, sep = "_")) 



## How many lat_long combinations are unique?
length(unique(all_stns_merged2$lat_long)) ##1113

## Find rows with the same lat_long
dup_lat_long2 <- all_stns_merged2 %>%
  group_by(lat_long) %>%
  filter(n() > 1) %>% # Keep only rows with more than one occurrence of the same lat_long
  select(study_area, location, lat_long, sensor_type) %>%  # Select relevant columns
  arrange(lat_long) ## BIO-TDN-021-07NR and BIO-TDN-021-7NR. Fix above in all_stns2 (added to line 452). Naming discrepancy fixed

## How many unique station names are in all_stns_merged2?
unique_stations2 <- unique(all_stns_merged2$location) ## 822 rows - so there are some duplicate station names still, indicating a lot of discrepancies in coordinates

### Compare lat/long values for rows that have the same location
duplicated_merged_stns2 <- all_stns_merged2 %>%
  group_by(location) %>%
  filter(n() > 1) %>% # Keep only rows with more than one occurrence of the same location
  select(study_area, location, lat_long, sensor_type) # Select relevant columns

## 582 duplicated stations!
## Rearrange duplicated_merged_stns2 df to have coord_aru and coord_cam columns and merge them into a single coord_lookup table
coord_lookup2 <- duplicated_merged_stns2 %>%
  mutate(coord_aru = ifelse(sensor_type == "aru", lat_long, NA),
         coord_cam = ifelse(sensor_type == "camera", lat_long, NA)) %>%
  select(study_area, location, coord_aru, coord_cam) %>%
  group_by(study_area, location) %>%
  summarise(coord_aru = paste(na.omit(coord_aru), collapse = ", "),
            coord_cam = paste(na.omit(coord_cam), collapse = ", ")) %>%
  ungroup()

## 291 stations where the coordinates differ between ARU and cameras....
## Occurs in all datasets?
table(coord_lookup2$study_area) # Not in Edehzhie - because that was all CWS
## Save this as a csv to compare to Google Drive
write.csv(coord_lookup2, "C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/NWTBM_point_locations/NWTBM_station_coordinate_discrepancies_July172025_v2.csv", row.names = FALSE)

### What is the distance between the two sets of coordinate for each station? This will help determine whether it's worth it to cross-reference
## Use geosphere package to calculate distances

## First split back into latitude and longitude columns
coord_lookup2 <- coord_lookup2 %>%
  separate(coord_aru, into = c("latitude_aru", "longitude_aru"), sep = "_", convert = TRUE) %>%
  separate(coord_cam, into = c("latitude_cam", "longitude_cam"), sep = "_", convert = TRUE)


## Use geosphere to calculate distances

coord_lookup2 <- coord_lookup2 %>%
  rowwise() %>% # Compute distances row by row
  mutate(distance_m = distHaversine(c(longitude_aru, latitude_aru), c(longitude_cam, latitude_cam))) %>% 
      ungroup()

summary(coord_lookup2$distance_m) ## Anywhere between 0.0048 and 12110.6 m... so 5mm to 12 km...
##  Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
##  4.800e-03 3.779e+00 9.604e+00 2.522e+02 1.950e+01 1.211e+04
class(coord_lookup2$distance_m)


hist(coord_lookup2$distance_m, breaks = 1000, main = "Distance between ARU and Camera Coordinates", xlab = "Distance (m)", ylab = "Frequency")
## Most (>75%) of differences are within 20m of each other. Median is 10m. For most analyses, this resolution is acceptable. But >20m differences may affect spatial variable extraction
## How many distances_m are greater than 20m?
table(coord_lookup2$distance_m > 20) ## 71 stations

big_coord_diffs <- coord_lookup2 %>%
  filter(distance_m > 20) ## significant (>10km) differences are in Fort Smith stations


## Save all_stns_merged for manual revision for names?
## write.csv(all_stns_merged, "C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/NWTBM_point_locations/NWTBM_all_sensor_locations_July162025.csv", row.names = FALSE)





