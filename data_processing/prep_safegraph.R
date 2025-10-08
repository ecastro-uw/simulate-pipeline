# Title: prep_safegraph.R - ARRAY JOB
# Author: Emma Castro
# Purpose: This script processes the Weekly Patterns SafeGraph data, which is expressed at the POI-week level, and
# summarizes the number of visits to restaurants and bars (separately) by US county and day (county-top category-day level).
# Instructions: Do not run interactively. Use launch_safegraph_jobs.txt to run this script in the command line.

# TO DO
# 1. Normalize visits 

## 0. SETUP -----------------------------------------------------------------------

# Load packages
library(data.table)
#source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# Define dirs
root <- '/ihme/limited_use/LIMITED_USE/PROJECT_FOLDERS/COVID19/SAFEGRAPH/'
input_dir <- '/ihme/scratch/users/ems2285/thesis/data/'
out_dir <- '/ihme/scratch/users/ems2285/thesis/intermediate/'

# Load location hierarchy (US counties)
#hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)
#counties <- hierarchy[level==3, .(location_id, fips = local_id)]
#counties[nchar(fips)==4, fips := paste0('0',fips)] #fix locs with leading zeros (N approx 300)
# N=32 counties in the hierarchy are missing a local_id

# Load mapping of SafeGraph place_id to top category
poi_info <- fread(paste0(out_dir,'poi_crosswalk.csv'))

# Load mapping of fips codes to county names
county_info <- fread(paste0(input_dir,'county_fips_codes_US_Census_Bureau_20230925.csv'),
                     colClasses = c("STATEFP"="character", 
                                    "COUNTYFP"="character"))
county_info <- county_info[, .(fips = paste0(STATEFP,COUNTYFP), state = STATE, county = COUNTYNAME)]


## 1. Generate a list of all Weekly Patterns files ----------------------------------
# There's one compressed csv file for each week of data 
old_visit_files <- list.files(paste0(root,'weekly_patterns/COMPRESSED_OLD/main-file/'), pattern = glob2rx('*-weekly-patterns.csv.gz'),
                              full.names = T)
new_visit_files <- list.files(paste0(root,'weekly_patterns/COMPRESSED/patterns/'), pattern = glob2rx('patterns-part*.csv.gz'),
                              full.names = T, recursive = T)
all_visit_files <- c(old_visit_files, new_visit_files)


## 2. Process the file ---------------------------------------------------------------

# Get task id and associated data file
task_id <- ifelse(Sys.getenv('SLURM_ARRAY_TASK_ID') != '', as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID')), NA)
filepath <- all_visit_files[task_id]

# Load desired file
df <- fread(filepath, #nrows = 1000, #nrows option for dev only
            select = c('safegraph_place_id', 'date_range_start', 'visits_by_day', 'poi_cbg'),
            colClasses = c("poi_cbg" = "character")) #must be treated as char so as not to lose leading 0s

# Convert cbg to fips
df[, fips := substr(poi_cbg,1,5)]

# Add top category and county information
# Note: inner join - are there bars/restaurants in Weekly Patterns that are not captured in the Core Places dataset? If so,
# they will be lost here. However, if they are not in Core Places, we have no way of identifying them as bars/restaurants.
df <- merge(df, poi_info[,.(safegraph_place_id, top_category)], by='safegraph_place_id') 

# Convert from weekly to daily space
df <- df[, week_start := as.Date(substr(date_range_start,1,10))]
daily_df <- df[, strsplit(gsub('\\[|]','', visits_by_day), ",", fixed=TRUE), by = .(safegraph_place_id, week_start, top_category, fips)]
setnames(daily_df, 'V1', 'visits')
daily_df[, id := seq_len(.N) - 1, by = safegraph_place_id]
daily_df[, date := week_start + id]

# Summarize by county, day, and top category 
visits_by_category <- daily_df[, .(visit_count = sum(as.numeric(visits))), by=.(top_category, fips, date)]

# Add state and county names
visits_by_category <- merge(visits_by_category, county_info, by='fips', all.x=T)

## 3. Save output ---------------------------------------------------------------------
fwrite(visits_by_category, paste0(out_dir, "weekly_files/daily_visits_",task_id,".csv"))
