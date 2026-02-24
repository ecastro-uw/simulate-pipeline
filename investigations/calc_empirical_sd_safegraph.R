# Calculate the "realized sd" statistic for real data

# Load packages 
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# Define dirs/file paths
input_data_path <- '/ihme/scratch/users/ems2285/thesis/intermediate/visits_by_cnty_day_cat_v3.csv'

# Load the location hierarchy
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)

# Which locations to run?
locs_dt <- hierarchy[parent_id==hierarchy[location_name=='Washington']$location_id, .(location_id, location_name)]

# Load mandate data
# Determine date of first imposition by county
events_dt <- fread(input_data_path)[location_id %in% locs_dt$location_id & top_category %like% 'Restaurants',
                                       .(location_id, date = as.Date(date), mandate = dining_close)][order(location_id, date)]
events_dt[, lag := shift(mandate, fill = 0), by=location_id]
events_dt <- events_dt[mandate != lag]
events_dt <- events_dt[, .SD[1], by = location_id]


# Load mobility data
mobility_dt <- fread(input_data_path)[location_id %in% locs_dt$location_id & top_category %like% 'Restaurants',
                                      .(location_id, fips, county, date = as.Date(date), y=visits_per_10k,
                                      mandate = dining_close)][date<max(events_dt$date)]

# Calculate mean weekly visits per 10K pop for the first 8 weeks of 2020
# for normalizing the data
baseline_dt <- mobility_dt[date>='2020-01-01' & date<'2020-02-26']
# check for missing data (8 wks x 7 days per week = 56 days)
if(nrow(baseline_dt[,.N,by=location_id][N!=56])==0){
  # summarize to week level and log transform
  baseline_dt <- baseline_dt[, week_id := rep(1:8, each=7), by=location_id]
  baseline_weekly <- baseline_dt[, .(y = log(sum(y))), by=c('location_id','week_id')]
  # calculate the mean
  baseline_weekly <- baseline_weekly[, .(mean_jan_feb = mean(y)), by=location_id]
}

# Subset to the 4 weeks leading up to the mandate
mobility_dt <- merge(mobility_dt, events_dt[,.(location_id, onset = date)])
mobility_dt <- mobility_dt[date >= (onset - 28) & date < onset]

# check that every location has data for each day of the window before proceeding
if(nrow(mobility_dt[,.N,by=location_id][N!=28])==0){
  # summarize to weekly level and log transform
  mobility_dt[, time_id := rep(-4:(-1), each=7), by=location_id]
  weekly_dt <- mobility_dt[, .(y = log(sum(y))), by=c('location_id','time_id')]
  # normalize
  weekly_dt <- merge(weekly_dt, baseline_weekly, by='location_id')
  weekly_dt[, y_norm := y / mean_jan_feb]
  # calculate difference
  weekly_dt <- weekly_dt[, delta := y_norm - shift(y_norm, type='lag'), by=location_id][! is.na(delta)]
  # calculate the sd of the deltas
  realized_sd <- sd(weekly_dt$delta)
}

