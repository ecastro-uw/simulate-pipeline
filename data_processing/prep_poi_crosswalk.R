# Title: prep_poi_crosswalk.R
# Author: Emma Castro
# Purpose: This script processes the Core Places SafeGraph data and creates a crosswalk that maps
# SafeGraph place_id to it's associated top category and the county in which its located. It should
# only need to be run once. The crosswalk will be saved here: \mnt\share\scratch\users\ems2285\thesis\intermediate

#Optional TODO - use lat/long to map each POI to it's county. helpful advice: https://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county
#Alternatively, can map POIs to county within the prep_safegraph script using census block group (cbg), which is included in the Weekly Patterns data but not the core places data.

# Load packages 
library(data.table)

# Define dirs
root <- '/ihme/limited_use/LIMITED_USE/PROJECT_FOLDERS/COVID19/SAFEGRAPH/'
out_dir <- '/ihme/scratch/users/ems2285/thesis/intermediate/'

# Define args
poi_release <- 'Core-USA-June2020-Release-CORE_POI-2020_05-2020-06-06' #the most recent release available internally, to my knowledge
poi_files <- c('core_poi-part1', 'core_poi-part2', 'core_poi-part3', 'core_poi-part4', 'core_poi-part5')

# Load location hierarchy (US counties)
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)

# Subset to columns of interest and filter to bars and restaurants only
poi_info <- data.table()
for (file in poi_files){
  df <- fread(paste0(root,'CORE_PLACES_OF_INTEREST/',poi_release,'/',file,'.csv'),
              select = c('safegraph_place_id', 'location_name', 'naics_code', 'top_category', 'sub_category', 'latitude', 'longitude',
                         'street_address', 'city', 'region', 'postal_code'))
  df <- df[substr(naics_code, 1, 4) %in% c(7224, 7225)]
  poi_info <- rbind(df, poi_info)
}


# TODO map each POI to a county and ihme location id (using zip code or lat/long)


# Output
fwrite(poi_info, paste0(out_dir,'poi_crosswalk.csv'))
