# Create location groups (context_ids) to be run through the pipeline together
# Outputs:
# (1) context_lookup_{suffix}.csv -  This file contains one row per context id and maps the ID to its definition.
#                                    It also maps the context_id to the associated candidate models.
# (2) locs_by_context_{suffix}.csv - This file lists all locations that belong to each context_id. It is in long format
#                                    (unique by context_id, location_id).


# Investigation 1:
# For each of the four imposition categories (1st/2nd restaurant/bar), group US counties
# by size (big/small) and political affiliation (rep/dem/mod). Additionally, for 2nd
# restaurant impositions, group imposition timing by Jun-Nov 2020 vs Dec 2020 - May 2021.

# Investigation 2:
# Location group definitions TBD

# Investigation 3:
# Location group definitions TBD

### (1) SETUP ###

# Define Args
suffix <-    'inv1'      # For distinguishing output file names
country <-   'USA'       # USA or Brazil
loc_units <- 'counties'  # States or counties
padding <-    2          # Discard 2 weeks of data after the mandate lifts

# Define dirs
input_root <- paste0('/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/',country,'_',loc_units,'/')
out_dir <- '/ihme/homes/ems2285/repos/simulate-pipeline/config_files/'

# Load population data and define bins (big/small)
pop_dt <- fread(file.path(input_root,'population.csv'))
pop_dt[, pop_cat := ifelse(pop>=100000, 'big', 'small')]

# Load political affiliation data and define bins (Dem/Mod/Rep)
elect_dt <- fread(file.path(input_root,'/election_results.csv'))
elect_dt[, pct := votes/total_votes]
elect_wide <- dcast(elect_dt, location_id ~ party, value.var = 'pct')
elect_wide[, pol_cat := ifelse(DEMOCRAT>0.55, 'D', ifelse(REPUBLICAN>0.55, 'R', 'M'))]


### (2) CREATE CONTEXT GROUPS ###

# Compile a mapping of each context and its constituent location ids
all_contexts <- data.table()
event_list <- c('first_restaurant', 'second_restaurant', 'first_bar', 'second_bar')
for (event in event_list){
  
  # Resolve imposition number
  mandate_num <- ifelse(substr(event,1,5)=='first','first','second')

  # Load the event timing data
  cols_to_keep <- c('location_id','onset_date')
  if(mandate_num=='second'){cols_to_keep <- c(cols_to_keep,'prev_lift')}
  event_dt <- fread(paste0(input_root, event,'_close.csv'))[,.SD, .SDcols=cols_to_keep]
  
  # Second impositions must occur at least 10 weeks after lifting of the previous mandate
  if(mandate_num=='second'){
    event_dt[, int_wks := as.numeric(floor((onset_date - prev_lift)/7))]
    event_dt <- event_dt[int_wks>=10]
  } 
  
  # Add population and politicial affiliation variables
  event_dt <- merge(event_dt, pop_dt[, .(location_id, pop_cat)], by='location_id')
  event_dt <- merge(event_dt, elect_wide, by='location_id')
  
  # Define mandate timing variable
  if(substr(event,1,5)=='first'){
    event_dt$timing_cat <- 'Mar-Apr 2020'
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
    
  # Generate a list of locations for each context (unique combo of event, timing, population, and political affiliation)
  if(mandate_num=='first'){
    context_map <- event_dt[, .(event = event,
                                timing_cat,
                                pop_cat,
                                pol_cat,
                                location_id,
                                onset_date)]
  } else {
    context_map <- event_dt[, .(event = event,
                                timing_cat,
                                pop_cat,
                                pol_cat,
                                location_id,
                                onset_date,
                                prev_lift,
                                int_wks)]
  }
  context_map <- context_map[order(event,timing_cat, pop_cat, pol_cat)]
  # Combine into one data table
  all_contexts <- rbind(all_contexts, context_map, fill=T)
}

# Ensure proper sorting before assigning context ids
all_contexts[, timing_cat := factor(timing_cat, levels=c("Mar-Apr 2020", "Jun-Nov 2020", "Dec 2020-May 2021", "Jun 2020-May 2021"))]
all_contexts[, event := factor(event, levels=c("first_restaurant", "second_restaurant", "first_bar", "second_bar"))]
all_contexts <- all_contexts[order(event, timing_cat, pop_cat, pol_cat)]

# Add a context ID
all_contexts[, context_id := .GRP, by=c('event', 'timing_cat', 'pop_cat', 'pol_cat')]


# Context definitions - create a file that is unique by context_id
context_guide <- all_contexts[,.N, by=c('context_id', 'event', 'timing_cat', 'pop_cat', 'pol_cat')]
context_guide[, `:=` (country = country,
                      ADMN = ifelse(loc_units=='states',1,2),
                      mandate_type = sub(".*_", "", event),
                      mandate_num = sub("_.*", "", event)
                 )]
context_guide[, outcome := paste0('visits to ', mandate_type, 's')]



### (3) DEFINE WHICH MODELS TO RUN FOR EACH CONTEXT ###


# 3(A) For each context, check which mandate covariates meet the inclusion criteria
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
  min_train_dt[, time_id := as.numeric(floor((date - onset_date)/7))]

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


# 3(B) For each context, check if case and death data meet the covariate inclusion criteria
cases_dt <- fread(paste0(input_root,'covid_cases_deaths.csv'))

check_cases_deaths <- function(context){
  
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
  
  # Subset the case & death data to the min training period for each location
  min_train_dt <- merge(cases_dt, one_context[,.(location_id, start_date, end_date, onset_date)],
                        by='location_id')
  min_train_dt <- min_train_dt[date>=start_date & date <=end_date]
  
  # Aggregate to the week level
  min_train_dt[, time_id := as.numeric(floor((date - onset_date)/7))]
  weekly_dt <- min_train_dt[, .(cases = sum(daily_cases),
                                deaths = sum(daily_deaths)),
                            by = c('location_id', 'time_id')]
  
  # Do cases and deaths meet the inclusion criteria?:
  # % of location-weeks with non-zero values >5%
  verdict <- data.table(
    context_id = context,
    cases = ifelse(sum(weekly_dt$cases>0)/nrow(weekly_dt)>0.05, 'X', '0'),
    deaths = ifelse(sum(weekly_dt$deaths>0)/nrow(weekly_dt)>0.05, 'X', '0')
  )

  return(verdict)
}

epi_covars <- rbindlist(lapply(unique(context_guide$context_id), check_cases_deaths))
context_lookup <- merge(mandate_covars, epi_covars, by='context_id')



# 3(C) Ready to resolve values for each model
context_lookup[, `:=` (
  model_1 = 1, # random walk
  model_2 = 1, # random walk with trend
  model_3 = 1, # linear AR model
  model_4 = ifelse(cases=='X',1,0),                  # AR + cases
  model_5 = ifelse(cases=='X' & deaths=='X',1,0),    # AR + cases + deaths
  model_6 = ifelse(edu=='X',1,0),                    # AR + schools
  model_7 = ifelse(gathering=='X',1,0),              # AR + gatherings
  model_8 = ifelse(gym=='X',1,0),                    # AR + gym
  model_9 = ifelse(bar=='X',1,0),                    # AR + bar
  model_10 = ifelse(gathering=='X' & bar=='X',1,0), # AR + gatherings + bars
  model_11 = ifelse(gathering=='X' & edu=='X',1,0),  # AR + gatherings + schools
  model_12 = ifelse(gathering=='X' & bar=='X' & edu=='X',1,0), # AR + gatherings + bars + schools
  model_13 = ifelse(gathering=='X' & bar=='X' & edu=='X' & gym=='X',1,0), # AR + gatherings + bars + schools + gym
  model_14 = 1, # ARIMA(1,1,0)
  model_15 = 1, # auto.arima
  model_16 = 0, # auto.arima + cases
  model_17 = 0, # auto.arima + mandate propensity
  model_18 = 0, # auto.arima + cases + deaths
  model_19 = 0, # auto.arima + cases + deaths + mandate propensity
  model_20 = 0  # neural network
)]


### (4) Final clean up and save to disk ####

# (a) context lookup
cols_to_keep <- c('context_id', 'country', 'ADMN', 'mandate_type', 'mandate_num',
                  'timing_cat', 'outcome', 'pop_cat', 'pol_cat', 'N', paste0('model_',1:20))
context_lookup <- context_lookup[, .SD, .SDcols = cols_to_keep]

fwrite(context_lookup, paste0(out_dir, 'context_lookup_',suffix,'.csv'))

# (b) locs by context
loc_map <- all_contexts[, .(context_id, location_id)]
fwrite(loc_map, paste0(out_dir,'locs_by_context_',suffix,'.csv'))


### Optional: Subset for testing/development purposes, as desired ####
if(F){
  subset <- context_lookup[mandate_type=='restaurant' & mandate_num=='second']
  loc_subset <- loc_map[context_id %in% subset$context_id]
  
  # update context ids
  subset[,context_id := context_id - 6]
  loc_subset[,context_id := context_id - 6]
  
  # save
  fwrite(subset, paste0(out_dir, 'context_lookup_testing.csv'))
  fwrite(loc_subset, paste0(out_dir, 'locs_by_context_testing.csv'))
}
