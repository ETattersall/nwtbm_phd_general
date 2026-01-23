###################
### 02_merge_station_locations.R
### Mapping station locations from datasets 
### included in fire-gamebird and fire-ungulate chapters
### Started 28 October 2024 by Erin Tattersall
### Updated with stations from wildtrax in May 2025
##################

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
       "geosphere",
       "cowplot",
       "leaflet",
       "terra", 
       "maptiles", 
       "ggplot2", 
       "tidyterra", 
       "ggspatial",
       "wildrtrax",
       "purrr",
       "tidyverse",
       "data.table") ## not all packages used in this script - holdover from older draft



## install.packages(x) ## should already be installed ####
lapply(x, require, character.only = TRUE)

#### issue: wildrtrax doesn't yet support authentication for Google API
## Needed to download station locations
remotes::install_github("ABbiodiversity/wildrtrax") ## May need to create a new personal access token to retrieve from github
##require(wildrtrax) <- added to package list above

## Authenticate into WildTrax. Access local script for WT_USERNAME and WT_PASSWORD 
source("wildtrax_login.R") ## This will set the environment variables WTUSERNAME and WTPASSWORD
wt_auth()



#### Station locations from wildtrax ####
# my_projects <- wt_get_download_summary(
#   sensor_id = "CAM"
# )
# 
# glimpse(my_projects)


### Until wildrtrax supports authentication for Google API, use location csvs manually downloaded from WildTrax
# July 16 - re-running with files from full data download, which have the same columns as the ARU location data
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/NWTBM_point_locations/WildTrax_camera/FromFullDataDownload")
list.files()

## Read CSV files into R and bind them together (note: added study area names to files manually)
## Note that these locations only refer to camera stations - if there are ARU only stations, they will not be included here
## add
# List csv files in the directory
cam_file_csv <- list.files(pattern = "\\.csv$") ## object listing all csvs (if re-running)

# Read and bind all CSVs, adding a column for the source file
tbl_fread <- rbindlist(lapply(cam_file_csv, function(file) {
  dt <- fread(file)
  dt[, source_file := basename(file)]
  return(dt)
}))

summary(tbl_fread) ## NAs in lat/long columns for gameti

### Add a column for study area
### Add a column for study area
tbl_fread <- tbl_fread %>%
  mutate(study_area = case_when(
    str_detect(source_file, "Edéhzhíe") ~ "Edéhzhíe",
    str_detect(source_file, "Fort_Smith") ~ "FortSmith",
    str_detect(source_file, "Gameti") ~ "Gameti",
    str_detect(source_file, "Norman_Wells") ~ "NormanWells",
    str_detect(source_file, "Sambaa_K'e") ~ "SambaaK'e",
    str_detect(source_file, "Thaidene_Nëné") ~ "ThaideneNëné",
    TRUE ~ NA_character_  # Default case if no match
  ))

## Remove source_file column
tbl_fread <- tbl_fread %>%
  select(-source_file)

glimpse(tbl_fread)

## Read in csv with Gameti station coords
getwd()
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/NWTBM_point_locations/NWTBM_GoogleDrive")
gam_tbl <- fread("Gameti_2023-2024_locations.csv")
class(gam_tbl)
glimpse(gam_tbl)
summary(gam_tbl) #80 rows, whereas tbl_fread shows 79 rows for Gameti -- digging through the project folder suggests one aru/camera station wasn't found (GSP-056-21)

gam_tbl_std <- gam_tbl %>%
  select(SSU, Latitude, Longitude) %>% # keep only relevant columns
  rename(location = SSU, latitude = Latitude, longitude = Longitude) # rename columns to match tbl_fread
summary(gam_tbl_std)


tbl_fread_gam <- tbl_fread %>%
  filter(study_area == "Gameti") %>% # filter for Gameti data
  select(location, latitude, longitude) # keep only relevant columns
summary(tbl_fread_gam)

## Find the rows in gam_tbl_std that aren't in tbl_fread_gam
gam_tbl_std %>%
  filter(!location %in% tbl_fread_gam$location) # 5 rows differ - interesting. Inspect raw datasheets
#     location latitude longitude
# 1: CRU-089-D3 64.07501 -116.2424 # D3, D5, D7 renamed to 03, 05, 07 in WildTrax camera projects, but not ARU projects
# 2: CRU-089-D5 64.08008 -116.2461
# 3: CRU-089-D7 64.08165 -116.2342
# 4: CRU-098-19 64.69679 -115.7259 # input as CRU-098-18 - error?
# 5: GSP-056-21 63.61512 -118.3970 # Sensors not retrieved 

## Going to go with what's in in the Google Drive, because that's what matches the ARU data
tbl_fread_gam %>%
  filter(!location %in% gam_tbl_std$location)

## Rename above stations to match tbl_fread_gam
tbl_fread[tbl_fread$location == "CRU-089-03", "location"] <- "CRU-089-D3"
tbl_fread[tbl_fread$location == "CRU-089-05", "location"] <- "CRU-089-D5"
tbl_fread[tbl_fread$location == "CRU-089-07", "location"] <- "CRU-089-D7"
tbl_fread[tbl_fread$location == "CRU-098-18", "location"] <- "CRU-098-19"



# Run code again to find different rows - now should just be the GSP-056-21 station (not retrieved so won't be included in analysis anyway)
gam_tbl_std %>%
  filter(!location %in% tbl_fread_gam$location)

## Now can add lat/longs to tbl_fread based on location names in gam_tbl_std
# Merge with suffixes to distinguish columns
merged <- merge(tbl_fread, gam_tbl_std, by = "location", suffixes = c("", ".ref"), all.x = TRUE) ## adds .ref columns for latitude and longitude from gam_tbl_std

# List of columns to fill
cols_to_fill <- c("latitude", "longitude")

# Fill NA values from .ref columns
for (col in cols_to_fill) {
  ref_col <- paste0(col, ".ref")
  merged[is.na(get(col)), (col) := get(ref_col)]
  merged[, (ref_col) := NULL] # Remove the .ref column
}


summary(merged) ## no more NAs in latitude and longitude columns
merged %>%
  filter(is.na(latitude) | is.na(longitude))
str(merged)
table(merged$study_area)
##      Edéhzhíe    FortSmith       Gameti  NormanWells    SambaaK'e ThaideneNëné 
##         151           60           79           80           29          307 

#Convert to a spatial vector, then to sf
stn_locations <- vect(merged,
                      geom = c("longitude", "latitude"),
                      crs = "EPSG:4326") # Note the CRS tells R that the projection is WGS1984
class(stn_locations)
# Convert stn_locations to sf object
stn_locations_sf <- st_as_sf(stn_locations)

#### Save station locations as shapefile ####
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/NWT_GIS_Data")
st_write(stn_locations_sf, "NWTBM_stations_locations_July162025.shp", delete_dsn = TRUE) # overwrite existing shapefile if it exists


### Also save merged station locations as CSV
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/NWTBM_point_locations/WildTrax_camera")

write.csv(merged, "NWTBM_stations_locations_July162025.csv", row.names = FALSE) # save as CSV without row names

## Read merged back in to compare with ARU locations
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/NWTBM_point_locations/WildTrax_camera")
list.files() ## want July 16 version because it has the right study area names and the right columns)
cam_stns <- read.csv("NWTBM_stations_locations_July162025.csv")
glimpse(cam_stns)
table(cam_stns$study_area)


#### Merge ARU station locations #### 
setwd("C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/NWTBM_point_locations/WildTrax_aru")
list.files() # 9 files (2 TDN projects, 3 Gameti projects, 1 for all others)

## Read CSV files into R and bind them together
# List csv files in the directory
aru_file_csv <- list.files(pattern = "\\.csv$") ## object listing all csvs

# Read and bind all CSVs, adding a column for the source file
aru_stns <- rbindlist(lapply(aru_file_csv, function(file) {
  dt <- fread(file)
  dt[, source_file := basename(file)]
  return(dt)
}))

glimpse(aru_stns)
summary(aru_stns) ## No lat/long NAs


## Check source file names
table(aru_stns$source_file)


### Add a column for study area
aru_stns <- aru_stns %>%
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

table(aru_stns$study_area)
table(is.na(aru_stns$study_area)) ## 0 NAs


## Remove source_file column
aru_stns <- aru_stns %>%
  select(-source_file)

## Will likely be some duplicates between Gameti files
aru_stns <- aru_stns[!duplicated(aru_stns), ] # Down from 886 to 742
glimpse(aru_stns)

## I know there are duplicate stations marked X in the aru TDN dataset. These are duplicate recordings uploaded to WildTrax and those stations can be removed
## Find rows where location includes _X
x_rows <- aru_stns %>%
  filter(str_detect(location, "_X")) ## 11 rows removed
## Remove these rows from aru_stns
aru_stns <- aru_stns %>%
  filter(!str_detect(location, "_X"))

## Save the ARU station locations
write.csv(aru_stns, "C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/NWTBM_point_locations/WildTrax_aru/NWTBM_aru_locations_July162025.csv", row.names = FALSE)

## Read back in ARU station locations (if env has been cleared)
aru_stns <- read.csv("C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/NWTBM_point_locations/WildTrax_aru/NWTBM_aru_locations_July162025.csv")

## Reorder both aru_stns and cam_stns first by study_area, then by location.
aru_stns <- aru_stns %>%
  arrange(study_area, location)


glimpse(aru_stns)

## How many NAs in aru study_area?
table(is.na(aru_stns$study_area)) ## 0 NAs in study_area


cam_stns <- cam_stns %>%
  arrange(study_area, location)

glimpse(cam_stns)

## How many NAs in cam study_area?
table(is.na(cam_stns$study_area)) ## 0 NAs in study_area

## Rearrange columns in cam_stns to match aru_stns
cam_stns <- cam_stns %>% 
  select(organization, location, location_id,location_buffer_m, latitude, longitude, elevation, location_visibility, location_comments, study_area)



## Add a column for sensor type
aru_stns$sensor_type <- "aru"

cam_stns$sensor_type <- "camera"

## rbind cam_stns and aru_stns. Duplicated Rows should indicate that the station has both camera and ARU sensors
all_stns <- rbind.data.frame(aru_stns, cam_stns)

summary(all_stns) ## No missing lat longs
## Any missing locations?
table(is.na(all_stns$location)) ## 0 NAs in location
## Any missing study_area?
table(is.na(all_stns$study_area)) ## 0 NAs in study_area


## Rearrange all_stns to order by study_area and location
all_stns <- all_stns %>%
  arrange(study_area, location)


## How many unique stations are in all_stns?
length(unique(all_stns$location)) ## 1053 unique station locations.



## Merge all rows in all_stns that have the same study_area, location name, latitude, and longitude.
all_stns_merged <- all_stns %>%
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



summary(all_stns_merged) ##1169 stations
table(all_stns_merged$study_area)
# How many NAs occur in study_area?
table(is.na(all_stns_merged$study_area)) ## 0 NAs in study_area
table(all_stns_merged$sensor_type) ## Merged 268 stations with matching study_area, location, latitude, and longitude. I don't think that many stations were aru or camera only, indicating discrepancies

## How many unique station names are in all_stns_merged?
unique_stations <- unique(all_stns_merged$location) ## 1053 rows - so there are some duplicate station names still, indicating discrepancies in coordinates

## Compare lat/long values for rows that have the same location
duplicated_merged_stns <- all_stns_merged %>%
  group_by(location) %>%
  filter(n() > 1) %>% # Keep only rows with more than one occurrence of the same location
  select(study_area, location, latitude, longitude, sensor_type) # Select relevant columns

## Rearrange duplicated_merged_stns df to have coord_aru and coord_cam columns and merge them into a single coord_lookup table
coord_lookup <- duplicated_merged_stns %>%
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
write.csv(coord_lookup, "C:/Users/tatterer.stu/Desktop/nwtbm_phd/data/NWTBM_point_locations/NWTBM_station_coordinate_discrepancies_July162025.csv", row.names = FALSE)

## Which rows have identical latitude AND longitude but different location names?
## Create a column combining latitude and longitude
all_stns_merged <- all_stns_merged %>%
  mutate(lat_long = paste(latitude, longitude, sep = "_"))

## How many lat_long combinations are unique?
length(unique(all_stns_merged$lat_long)) ## 1113 - so 56 that are the same (1169 - 1113)

## Find rows with the same lat_long
dup_lat_long <- all_stns_merged %>%
  group_by(lat_long) %>%
  filter(n() > 1) %>% # Keep only rows with more than one occurrence of the same lat_long
  select(study_area, location, lat_long, sensor_type) %>%  # Select relevant columns
  arrange(lat_long)

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





