# Title: combine_weeks.R
# Author: Emma Castro
# Purpose: Read in all intermediate files (county-day-top category level), each of which contains one week's worth of
# daily data, and combine into a single file. Add mandate time series. Sort and save.

# Load packages
library(data.table)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# Defined directories and filepaths
root <- "/ihme/scratch/users/ems2285/thesis/intermediate/"
mandate_dir <- "/snfs1/Project/covid/data_intake_prod/social distancing/diagnostic outputs/alternate_prepped_outputs/prepped_outputs_counties/"
pop_path <- "/mnt/share/scratch/users/ems2285/thesis/data/census_county_est2023_alldata.csv"


## Prep hierarchy ---
# Load US counties hierarchy
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)
# loc_id to fips maping
counties <- hierarchy[level==3, .(location_id, fips = local_id)]
counties[location_id==891, location_id:=531] #Washington DC
# state-county-id mapping
states_and_counties <- merge(hierarchy[level==3, .(location_id, parent_id, county=location_name)],
                             hierarchy[level==2, .(location_id, state = location_name)],
                             by.x = "parent_id", by.y = "location_id")

# Fix locs with leading zeros (N approx 300) - not necessary
#counties[nchar(fips)==4, fips := paste0('0',fips)] 
# TODO Fix N=32 counties in the hierarchy are missing a local_id


## Prep visit data ---
# Generate a list of all intermediate files
all_files <- list.files(paste0(root,'weekly_files'), full.names = T)

# Combine all weekly files into a single data.table
data <- rbindlist(lapply(all_files, fread))

# Subset to just the 50 states + DC (remove Guam, Puerto Rico, Virgin Islands, and NA)
#TODO - better method that doesnt require merging on census fips to name mapping csv (in prep_safegraph.R)
data <- data[! state %in% c('GU', 'PR', 'VI', '')]

# Address duplicate location-dates
# In some cases, one week of data is stored across 4 csv files, each of which has been summarized at the
# location-day-category level. So, the visit totals for duplicate rows need to be summed together to make the 
# file unique by loc-day-cat.
data <- data[, sum(visit_count), by=c('fips', 'top_category', 'date', 'state', 'county')]
setnames(data, 'V1', 'visit_count')

# Add IHME location ids
data$fips <- as.character(data$fips)
data <- merge(data, counties, by='fips', all.x=T)
# 60 fips codes in the data dont have a location_id. 
# E.g. Miami-Dade is in the IHME hierarchy but doesnt have a local-id
# E.g. Maui County is included in the IHME hierarchy as "Kalawao County, Maui County" and doesnt have a local-id
#TODO make a csv to manually fill in missing fips codes


## Add population information ---

# Load the raw data file
#pop_dt <- as.data.table(read.csv(pop_path, fileEncoding='latin1'))
#pop_dt <- pop_dt[,.(state=STNAME, county=CTYNAME, pop=POPESTIMATE2020)][state!=county]

# Make names compatible with ihme loc hierarchy
#pop_dt[, county:=tolower(county)]
#pop_dt[, county:=gsub('st\\.', "saint", county)]

# Add population to the SG data
#data <- merge(data, pop_dt, by='location_id', all.x=T)

# Calculate visits per 10K
#data[, visits_per_10k := (visit_count/pop)*10000]


## Add mandate information ---
# Load dining closures
dining_dt <- fread(paste0(mandate_dir,'dining_close.csv'))[,.(location_id, date, dining_close)]
dining_dt[, date := as.Date(date, format='%d.%m.%Y')]

# Load bar closures
bar_dt <- fread(paste0(mandate_dir,'bar_close.csv'))[,.(location_id, date, bar_close)]
bar_dt[, date := as.Date(date, format='%d.%m.%Y')]

# Merge
data <- merge(data, dining_dt, by=c('location_id', 'date'), all.x=T)
data <- merge(data, bar_dt, by=c('location_id','date'), all.x=T)

# NOTES
# 1. 60 FIPS are missing mandate information because they don't have a valid location id (flagged above)

# Backfill NAs with 0s
data[date<'2020-01-01', `:=` (dining_close = 0, bar_close = 0)]

# Output
fwrite(data, paste0(root,'visits_by_cnty_day_cat_v2.csv'))


