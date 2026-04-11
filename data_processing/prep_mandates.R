# Process Mandate Data

source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# Resolve args
country <- 'USA' #USA or Brazil
loc_resolution <- 'counties' #states or counties
mandate_type <- 'dining' #dining or bar
var_name <- paste0(mandate_type,'_close')
in_file_name <- paste0(var_name,'.csv')
out_file_name <- ifelse(mandate_type=='dining', 'restaurant', mandate_type)

# Define dirs
if (country=='USA' & loc_resolution=='counties'){
  # US counties
  input_root <- "/snfs1/Project/covid/data_intake_prod/social distancing/diagnostic outputs/alternate_prepped_outputs/prepped_outputs_counties/"
} else if (loc_resolution=='states'){
  # US and BR states
  input_root <- "/snfs1/Project/covid/data_intake_prod/social distancing/prepped_outputs/"
} else {
  # BR municipalities
  input_root <- "/snfs1/Project/covid/data_intake_prod/social distancing/diagnostic outputs/alternate_prepped_outputs/prepped_outputs_brazil/"
}
out_root <- "/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/"

# Load hierarchy
if (country=='USA'){
  hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)
  if(loc_resolution=='counties'){
    # US counties
    loc_list <- hierarchy[level==3, location_id]
  } else {
    # US states
    loc_list <- hierarchy[level==2, location_id]
  }
} else if (country=='Brazil' & loc_resolution=='states'){
  hierarchy <- get_location_metadata(location_set_id = 35, release_id = 9)
  loc_list <- hierarchy[parent_id==135, location_id]
}  

# Load the data
dt <- fread(file.path(input_root, in_file_name))[location_id %in% loc_list, .(location_id, date, mandate = get(var_name))]
dt[, date := as.Date(date, format='%d.%m.%Y')]

# Summarize implementation and lift dates
events_dt <- copy(dt)
events_dt <- dt[order(location_id, date)]
events_dt <- events_dt[, lag := shift(mandate, fill = 0), by=location_id][mandate != lag]
events_dt <- events_dt[, lift := shift(date, type="lead"), by=location_id][mandate==1]
events_dt <- events_dt[,.(location_id, onset_date = date, lift)]
events_dt <- events_dt[, prev_lift := shift(lift, type="lag"), by=location_id]
events_dt[, num := seq_len(.N), by = location_id]

# First implementation
first_close <- events_dt[num==1][, .(location_id, onset_date, lift)]
fwrite(first_close, paste0(out_root,country,'_',loc_resolution,'/first_',out_file_name,'_close.csv'))

# Second implementation
second_close <- events_dt[num==2][, .(location_id, prev_lift, onset_date, lift)]
# Remove Nebraska ("first imposition" was only in effect for one day (in most cases) 
# and then "second" mandate was re-imposed 1-4 days later)
if(country=='United States' & loc_resolution=='counties' & mandate_type=='dining'){
  second_close <- second_close[! location_id %in% hierarchy[parent_id==550, location_id]]
}
fwrite(second_close, paste0(out_root,country,'_',loc_resolution,'/second_',out_file_name,'_close.csv'))

# Third implementation
third_close <- events_dt[num==3][, .(location_id, prev_lift, onset_date, lift)]
fwrite(third_close, paste0(out_root,country,'_',loc_resolution,'/third_',out_file_name,'_close.csv'))


# Other Mandate Time Series
one_mandate <- function(mandate_name){
  temp_dt <- fread(paste0(input_root, mandate_name, '.csv'))[location_id %in% loc_list,
                                                            .(location_id, date, mandate=get(mandate_name))]
  temp_dt[, date := as.Date(date, format='%d.%m.%Y')]
  temp_dt <- temp_dt[date<'2022-01-01']
  setnames(temp_dt, 'mandate', mandate_name)
  return(temp_dt)
}

edu <- one_mandate('primary_edu')
gathering <- one_mandate('gatherings50i100o')
gym <- one_mandate('gym_pool_leisure_close')
retail <- one_mandate('non_essential_retail_close')
sah <- one_mandate('stay_at_home')
dining <- one_mandate('dining_close')
bar <- one_mandate('bar_close')

all_mandates <- merge(edu, gathering, by=c('location_id','date'))
all_mandates <- merge(all_mandates, gym, by=c('location_id','date'))
all_mandates <- merge(all_mandates, retail, by=c('location_id','date'))
all_mandates <- merge(all_mandates, sah, by=c('location_id','date'))
all_mandates <- merge(all_mandates, dining, by=c('location_id','date'))
all_mandates <- merge(all_mandates, bar, by=c('location_id','date'))

fwrite(all_mandates, paste0(out_root,country,'_',loc_resolution,'/other_mandate_time_series.csv'))
