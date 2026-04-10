# Calculate size of location groups

# For each of the four imposition categories (1st/2nd restaurant/bar), group counties
# by size (big/small), political affiliation (rep/dem/mod), and mandate timing.

# Load packages/funcs
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# Load hierarchy
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)

# Define dirs
input_root <- '/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/USA_counties/'

# Define args
mandate_type <- 'bar'
mandate_num <- 'second'
padding <- 2 #discard 2 weeks of data after the mandate lifts


# load population data
#mcnty_pop <- fread("/mnt/share/dex/us_county/03_post_model/pop_denom/best/inputs/county_population_age_sex.csv")[year_id == 2020][, sum(pop, na.rm = T), by = 'mcnty']
#setnames(mcnty_pop,'V1','pop')
#TODO - need mcounty to location_id mapping
pop_dt <- fread("/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/USA_counties/population.csv")
pop_dt[, pop_cat := ifelse(pop>=100000, 'big', 'small')]

# load political affiliation data
elect_dt <- fread("/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/USA_counties/election_results.csv")
elect_dt[, pct := votes/total_votes]
elect_wide <- dcast(elect_dt, location_id ~ party, value.var = 'pct')
elect_wide[, pol_cat := ifelse(DEMOCRAT>0.55, 'D', ifelse(REPUBLICAN>0.55, 'R', 'M'))]

# load event timing data
event_dt <- fread(paste0(input_root, mandate_num,'_',mandate_type,'_close.csv')) 

# For second impositions, drop events with insufficient interval between first lift and second imposition
if(mandate_num=='second'){
  event_dt <- event_dt[floor((onset_date - prev_lift)/7)>=10]
}

# combine data
event_dt <- merge(event_dt[,.(location_id, onset_date)], pop_dt[, .(location_id, pop_cat)], by='location_id', all.x=T)
event_dt <- merge(event_dt, elect_wide, by='location_id', all.x=T)

# check for missingness
print(paste(nrow(event_dt[is.na(pop_cat)]), "counties are missing population data"))
print(paste(nrow(event_dt[is.na(pol_cat)]), "counties are missing political party data"))
event_dt <- event_dt[! is.na(pop_cat) & ! is.na(pol_cat)]

# categorize timing
if(mandate_num=='first'){
  
  # Define timing bin
  event_dt$timing_cat <- 'Mar-Apr 2020'
  
  # Calc number of locations per bin
  summary <- event_dt[,.N, by=c('pop_cat', 'pol_cat')]
  summary_wide <- dcast(summary, pop_cat ~ pol_cat, value.var = 'N')
}

if(mandate_num=='second'){
  
  # Define timing bin
  event_dt[, month := month(onset_date)]
  event_dt[, year := year(onset_date)]
  if(mandate_type=='restaurant'){
    event_dt[, timing_cat := ifelse(year==2020 & month %in% 6:11, 'Jun-Nov 2020',
                                    ifelse((year==2020 & month==12) | (year==2021 & month %in% 1:5), 'Dec 2020-May 2021', NA))]
  } else {
    event_dt$timing_cat <- 'Jun 2020-May 2021'
  }
  # Calc number of locations per bin
  summary <- event_dt[,.N, by=c('timing_cat', 'pop_cat', 'pol_cat')]
  summary_wide <- dcast(summary[! is.na(timing_cat)], 
                        pop_cat ~ timing_cat + pol_cat)
}



### PART 2 ###
# Compile a mapping of each context and its constituent location ids
all_contexts <- data.table()
event_list <- c('first_restaurant', 'second_restaurant', 'first_bar', 'second_bar')
for (event in event_list){
  mandate_num <- ifelse(substr(event,1,5)=='first','first','second')

  # Load the event file
  cols_to_keep <- c('location_id','onset_date')
  if(mandate_num=='second'){cols_to_keep <- c(cols_to_keep,'prev_lift')}
  event_dt <- fread(paste0(input_root, event,'_close.csv'))[,.SD, .SDcols=cols_to_keep]
  
  # Second impositions must occur at least 10 weeks after lifting of the previous mandate
  if(mandate_num=='second'){
    event_dt[, int_wks := floor((onset_date - prev_lift)/7)]
    event_dt <- event_dt[int_wks>=10]
  } else {
    event_dt$int_wks <- NA
  }
  
  # Add population and election categories
  event_dt <- merge(event_dt, pop_dt[, .(location_id, pop_cat)], by='location_id')
  event_dt <- merge(event_dt, elect_wide, by='location_id')
  
  # add timing bin
  if(substr(event,1,5)=='first'){
    event_dt$timing_cat <- 'Mar-Apr 2020'
    event_dt$prev_lift <- NA
  } else{
    if(event=='second_restaurant'){
      event_dt[, month := month(onset_date)]
      event_dt[, year := year(onset_date)]
      event_dt[, timing_cat := ifelse(year==2020 & month %in% 6:11, 'Jun-Nov 2020',
                                    ifelse((year==2020 & month==12) | (year==2021 & month %in% 1:5), 'Dec 2020-May 2021', NA))]
    } else {
      event_dt$timing_cat <- 'Jun 2020-May 2021'
    }
  }
    
  # Generate a list of locations for each context (unique combo of event, timing, population, and political affilation)
  context_map <- event_dt[, .(event = event,
                              timing_cat,
                              pop_cat,
                              pol_cat,
                              location_id,
                              onset_date,
                              prev_lift,
                              int_wks
  )]
  context_map <- context_map[order(event,timing_cat, pop_cat, pol_cat)]
  all_contexts <- rbind(all_contexts, context_map)
  
}

# Add a context ID
all_contexts[, context_id := .GRP, by=c('event', 'timing_cat', 'pop_cat', 'pol_cat')]

# Check bin sizes for each
all_contexts[,.N, by=context_id]

# Context definitions
context_guide <- unique(all_contexts[, .(context_id, event, timing_cat, pop_cat, pol_cat)])

# For each context, check which mandate covariates meet the inclusion criteria
mandates <- fread(paste0(input_root,'other_mandate_time_series.csv'))

check_mandates <- function(context){
  
  # Get info for the context of interest
  one_context <- all_contexts[context_id==context]
  N <- nrow(one_context)
  mandate_num <- ifelse(substr(unique(one_context$event),1,5)=='first','first','second')
  
  # Determine beginning and end of the smallest training period
  if(mandate_num=='first'){
    one_context[ , start_date := onset_date - (8*7)] #8 weeks prior to onset
    one_context[ , end_date := start_date + (5*7) - 1] #min train period is 5 weeks
  } else{
    one_context[, max_train_length := int_wks - padding]
    one_context[, min_train_length := max_train_length - 3]
    one_context[, start_date := onset_date - max_train_length*7]
    one_context[, end_date := start_date + (min_train_length*7) - 1]
  }
  
  # Subset the mandate time series to the min training period for each location
  min_train_dt <- merge(mandates, one_context[,.(location_id, start_date, end_date, onset_date)],
                    by='location_id')
  min_train_dt <- min_train_dt[date>=start_date & date <=end_date]

  # aggregate to the week level
  min_train_dt[, time_id := floor((date - onset_date)/7)]

  weekly_dt <- min_train_dt[, .(pct_edu = sum(primary_edu)/7,
                                pct_gathering = sum(gatherings50i100o)/7,
                                pct_gym = sum(gym_pool_leisure_close)/7,
                                pct_retail = sum(non_essential_retail_close)/7,
                                pct_sah = sum(stay_at_home)/7,
                                pct_dining = sum(dining_close)/7,
                                pct_bar = sum(bar_close)/7),
                            by = c('location_id', 'time_id')]
  col_names <- c('pct_edu','pct_gathering','pct_gym','pct_retail','pct_sah','pct_dining','pct_bar')
  
  # Calculate activity level for each mandate. Which ones meet the minimum criteria?
  
  # % of location-weeks with mandate in effect is between 10% and 90%
  result <- weekly_dt[, lapply(.SD, function(x) mean(x == 1)), .SDcols = col_names]
  verdict <- result[, lapply(.SD, function(x) ifelse(x>0.1 & x<0.9,'X','0')), .SDcols = col_names]
  setnames(verdict, col_names, gsub('pct_','',col_names))
  
  verdict$context_id <- context
  return(verdict)
}

mandate_covars <- rbindlist(lapply(unique(context_guide$context_id), check_mandates))
mandate_covars <- merge(context_guide, mandate_covars, by='context_id')

# If the event is a restaurant mandate, do not consider restaurant mandates as a covariate option
mandate_covars[grepl("restaurant",event), dining:=0]
# If the event is a bar mandate, do not consider bar mandates as a covariate option
mandate_covars[grepl("bar",event), bar:=0]

fwrite(mandate_covars, file.path(input_root, 'mandate_covars_by_context.csv'))


# output a mapping of location_ids belonging to each context ID
loc_map <- all_contexts[, .(context_id, location_id)]

fwrite(loc_map, '/ihme/scratch/users/ems2285/thesis/inputs/config_files/locs_by_context.csv')
