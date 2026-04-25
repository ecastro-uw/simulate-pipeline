# Create location groups (context_ids) to be run through the pipeline together
# Outputs:
# (1) context_lookup_{suffix}.csv     - One row per context id; maps the ID to its definition
#                                       and to the associated candidate models.
# (2) locs_by_context_{suffix}.csv    - Long-format mapping of location_id to context_id.
# (3) dropped_locations_{suffix}.csv  - Log of locations dropped during QC and the reason.


# Investigation 1:
# For each of the four imposition categories (1st/2nd restaurant/bar), group US counties
# by size (big/small) and political affiliation (rep/dem/mod). 

# Investigation 2:
# Location group definitions TBD

# Investigation 3:
# Location group definitions TBD

library(data.table)
library(lubridate)

### (1) SETUP ###

# --- Args ---
suffix    <- 'inv1_0425'     # For distinguishing output file names
country   <- 'USA'      # USA or Brazil
loc_units <- 'counties' # states or counties
data_source <- 'safegraph'
padding   <-  2         # Weeks of data to discard after a mandate lifts

# --- Paths ---
input_root <- paste0('/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/', country, '_', loc_units, '/')
out_dir    <- '/ihme/homes/ems2285/repos/simulate-pipeline/config_files/'

# --- Thresholds ---
pop_threshold    <- 100000  # Counties >= this are classified "big"
pol_threshold    <- 0.55    # Party vote share above this → D or R; otherwise M
min_interval_wks <- 10      # Second imposition must be >= this many weeks after first lift
padding          <- 2       # Waiting period after first lift
max_train_wks    <- 8       # Maximum training window length for first impositions (weeks)
min_train_wks    <- 6       # Minimum training window length for first impositions (weeks)
min_train_flex   <- 2       # Flexibility window around minimum for second impositions (weeks)
#mandate_lo       <- 0.1     # Mandate must be active in > this fraction of location-weeks to be eligible
#mandate_hi       <- 0.9     # Mandate must be active in < this fraction of location-weeks to be eligible
#epi_threshold    <- 0.05    # Cases/deaths must be non-zero in > this fraction of location-weeks to be eligible


# Load population data and define bins (big/small)
pop_dt <- fread(file.path(input_root, 'population.csv'))
pop_dt[, pop_cat := ifelse(pop >= pop_threshold, 'big', 'small')]

# Load political affiliation data and define bins (D/M/R)
elect_dt   <- fread(file.path(input_root, 'election_results.csv'))
elect_dt[, pct := votes / total_votes]
elect_wide <- dcast(elect_dt, location_id ~ party, value.var = 'pct')
elect_wide[, pol_cat := ifelse(DEMOCRAT > pol_threshold, 'D', ifelse(REPUBLICAN > pol_threshold, 'R', 'M'))]


### (2) CREATE CONTEXT GROUPS ###

drop_log     <- list()
all_contexts <- data.table()
event_list   <- c('first_restaurant', 'second_restaurant', 'first_bar', 'second_bar')

for (event in event_list) {
  
  # Load the event data
  mandate_num  <- ifelse(substr(event, 1, 5) == 'first', 'first', 'second')
  cols_to_keep <- c('location_id', 'onset_date')
  if (mandate_num == 'second') cols_to_keep <- c(cols_to_keep, 'prev_lift')
  event_dt <- fread(paste0(input_root, event, '_close.csv'))[, .SD, .SDcols = cols_to_keep]

  # Second impositions must occur at least min_interval_wks weeks after lifting of the previous mandate
  if (mandate_num == 'second') {
    event_dt[, int_wks := as.numeric(floor((onset_date - prev_lift) / 7))]
    dropped <- event_dt[int_wks < min_interval_wks, location_id]
    if (length(dropped) > 0) {
      drop_log[[paste0(event, '__short_interval')]] <- data.table(
        location_id = dropped, event = event, reason = 'short_interval'
      )
    }
    event_dt <- event_dt[int_wks >= min_interval_wks]
  }
  
  # Load mobility data
  if (data_source == 'safegraph') {
    cat_name <- ifelse(grepl('restaurant', event), 'Restaurants', 'Drinking')
    outcome_dt <- fread(paste0(input_root,'/processed_safegraph_data.csv'))[top_category %like% cat_name, .(location_id, date, v = visit_count)]
    outcome_dt <- outcome_dt[! is.na(v)]
    setorder(outcome_dt, location_id, date)
  } else if (data_source == 'google') {
    outcome_dt <- fread(paste0(input_root,'/google_mobility.csv'))[, .(location_id, date, v = retail_and_recreation_percent_change_from_baseline)]
    setorder(outcome_dt, location_id, date)
  }
    
  # Check for locations missing from the mobility file
  no_mob <- setdiff(event_dt$location_id, outcome_dt$location_id)
  if (length(no_mob) > 0) {
    drop_log[[paste0(event, '__no_mob')]] <- data.table(
      location_id = no_mob, event = event, reason = 'no_mobility_data'
    )
  }
  event_dt <- event_dt[! location_id %in% no_mob]
  outcome_dt <- outcome_dt[location_id %in% event_dt$location_id]
  
  # Check for missing data in the baseline period
  if(data_source=='safegraph'){
    baseline_dt <- outcome_dt[date >= '2020-01-03' & date <= '2020-02-06']
    complete_baseline <- baseline_dt[!is.na(v), .N, by = location_id][N == 35, location_id]
    incomplete_baseline <- setdiff(unique(event_dt$location_id), complete_baseline)
    if (length(incomplete_baseline) > 0) {
      drop_log[[paste0(event, '__incomplete_baseline')]] <- data.table(
        location_id = incomplete_baseline, event = event, reason = 'incomplete_baseline'
      )
    }
    event_dt <- event_dt[! location_id %in% incomplete_baseline]
    outcome_dt <- outcome_dt[location_id %in% event_dt$location_id]
  }
  
  # Check for missing data in the analysis period
  outcome_dt <- merge(outcome_dt, event_dt, by='location_id')
  if (mandate_num=='first'){
    outcome_dt <- outcome_dt[date >= (onset_date - (max_train_wks * 7L)) &
                               date < (onset_date + 7L)]
    outcome_dt[, time_id := as.numeric(floor((date - onset_date) / 7)), by = location_id]
    
    # Check if any locations having missing weeks
    missing_weeks <- outcome_dt[, length(unique(time_id)), by=location_id][V1 < (max_train_wks+1)]
    # Check if any locations have missing days (each time_id should have 7 rows associated with it)
    missing_days <- outcome_dt[, .N, by=c('location_id', 'time_id')][N!=7]
    # Combine
    missing_mob <- unique(c(missing_weeks$location_id, missing_days$location_id))
  } else{
    outcome_dt[, start_date := {
      base_date <- prev_lift + padding * 7L
      onset_dow  <- wday(onset_date)
      base_dow   <- wday(base_date)
      days_to_add <- (onset_dow - base_dow) %% 7
      base_date + days_to_add
    }]
    outcome_dt <- outcome_dt[date >= start_date & date < (onset_date + 7L)]
    outcome_dt[, time_id := as.numeric(floor((date - onset_date) / 7)), by = location_id]
    
    # Check if the onset date extends beyond the available data
    onset_after_data_ends <- outcome_dt[, max(time_id), by=location_id][V1<0]
    # Check if any locations have missing days (each time_id should have 7 rows associated with it)
    missing_days <- outcome_dt[, .N, by=c('location_id', 'time_id')][N!=7]
    # Combine
    missing_mob <- unique(c(onset_after_data_ends$location_id, missing_days$location_id))
  }
  if (length(missing_mob) > 0) {
    drop_log[[paste0(event, '__missing_mob')]] <- data.table(
      location_id = missing_mob, event = event, reason = 'missing_mobility'
    )
  }
  event_dt <- event_dt[! location_id %in% missing_mob]

  
  # Add population category; log locations absent from pop_dt
  missing_pop <- setdiff(event_dt$location_id, pop_dt$location_id)
  if (length(missing_pop) > 0) {
    drop_log[[paste0(event, '__missing_pop')]] <- data.table(
      location_id = missing_pop, event = event, reason = 'missing_population'
    )
  }
  event_dt <- merge(event_dt, pop_dt[, .(location_id, pop_cat)], by = 'location_id')

  # Add political affiliation category; log locations absent from elect_wide
  missing_pol <- setdiff(event_dt$location_id, elect_wide$location_id)
  if (length(missing_pol) > 0) {
    drop_log[[paste0(event, '__missing_pol')]] <- data.table(
      location_id = missing_pol, event = event, reason = 'missing_election_data'
    )
  }
  event_dt <- merge(event_dt, elect_wide[, .(location_id, pol_cat)], by = 'location_id')

  #TODO - for 2nd mandates, check for onset dates occurring after end of mobility data availability

  # Collect relevant columns and append to all_contexts
  cols_out <- c('location_id', 'onset_date', 'pop_cat', 'pol_cat')
  if (mandate_num == 'second') cols_out <- c(cols_out, 'prev_lift', 'int_wks')
  context_map        <- event_dt[, .SD, .SDcols = cols_out]
  context_map[, event := event]
  all_contexts       <- rbind(all_contexts, context_map, fill = TRUE)
}


# Ensure proper sorting before assigning context ids
all_contexts[, event      := factor(event,      levels = c('first_restaurant', 'second_restaurant', 'first_bar', 'second_bar'))]
all_contexts <- all_contexts[order(event, pop_cat, pol_cat)]

# Assign context IDs
all_contexts[, context_id := .GRP, by = c('event', 'pop_cat', 'pol_cat')]

# Build context definitions (one row per context)
context_guide <- all_contexts[, .N, by = c('context_id', 'event', 'pop_cat', 'pol_cat')]
context_guide[, `:=`(
  country      = country,
  ADMN         = ifelse(loc_units == 'states', 1, 2),
  mandate_type = sub(".*_", "", event),
  mandate_num  = sub("_.*", "", event)
)]
context_guide[, outcome := paste0('visits to ', mandate_type, 's')]


### (3) DEFINE WHICH MODELS TO RUN FOR EACH CONTEXT ###

# Helper: add per-location training window dates to a context subset (modifies in place)
add_train_window <- function(dt, mandate_num) {
  if (mandate_num == 'first') {
    dt[, start_date := onset_date - max_train_wks * 7L]
    dt[, end_date   := start_date + min_train_wks * 7L - 1L]
  } else {
    dt[, max_train_length := int_wks - padding]
    dt[, min_train_length := max_train_length - min_train_flex]
    dt[, start_date       := onset_date - max_train_length * 7L]
    dt[, end_date         := start_date + min_train_length * 7L - 1L]
  }
  dt
}

# 3(A) Check which mandate covariates meet the inclusion criteria for each context
mandates <- fread(paste0(input_root, 'other_mandate_time_series.csv'))

check_mandates <- function(context) {
  one_context <- all_contexts[context_id == context]
  mandate_num <- ifelse(substr(unique(one_context$event), 1, 5) == 'first', 'first', 'second')
  one_context <- add_train_window(one_context, mandate_num)

  min_train_dt <- merge(mandates, one_context[, .(location_id, start_date, end_date, onset_date)],
                        by = 'location_id')
  min_train_dt <- min_train_dt[date >= start_date & date <= end_date]
  min_train_dt[, time_id := as.numeric(floor((date - onset_date) / 7))]

  col_names <- c('pct_edu', 'pct_gathering', 'pct_gym', 'pct_retail', 'pct_sah', 'pct_dining', 'pct_bar')
  weekly_dt <- min_train_dt[, .(pct_edu        = sum(primary_edu) / 7,
                                pct_gathering  = sum(gatherings50i100o) / 7,
                                pct_gym        = sum(gym_pool_leisure_close) / 7,
                                pct_retail     = sum(non_essential_retail_close) / 7,
                                pct_sah        = sum(stay_at_home) / 7,
                                pct_dining     = sum(dining_close) / 7,
                                pct_bar        = sum(bar_close) / 7),
                            by = c('location_id', 'time_id')]
  
  # lag by one week to reflect the model covariate
  weekly_dt <- weekly_dt[, 
                         (col_names) := lapply(.SD, shift), 
                         by = location_id, 
                         .SDcols = col_names
  ][!is.na(pct_edu)]

  # If there is any variation, include as a covariate
  result <- weekly_dt[, lapply(.SD, function(x) length(unique(x))), .SDcols = col_names]
  verdict <- result[, lapply(.SD, function(x) ifelse(x > 1, 1, 0)), .SDcols = col_names]
  
  # old criteria: 10-90% of location-weeks with mandate in effect the whole week
  #result  <- weekly_dt[, lapply(.SD, function(x) mean(x == 1)), .SDcols = col_names]
  #verdict <- result[, lapply(.SD, function(x) ifelse(x > mandate_lo & x < mandate_hi, 1, 0)), .SDcols = col_names]
  
  setnames(verdict, col_names, gsub('pct_', '', col_names))
  verdict$context_id <- context
  return(verdict)
}

mandate_covars <- rbindlist(lapply(unique(context_guide$context_id), check_mandates))

# Add context guide info
mandate_covars <- merge(context_guide, mandate_covars, by = 'context_id')

# Exclude same-type mandate as a covariate (avoid collinearity with the outcome)
mandate_covars[grepl("restaurant", event), dining := 0]
mandate_covars[grepl("bar",        event), bar    := 0]

# 3(B) Check if case and death data meet the covariate inclusion criteria for each context
cases_dt <- fread(paste0(input_root, 'covid_cases_deaths.csv'))

check_cases_deaths <- function(context) {
  one_context <- all_contexts[context_id == context]
  mandate_num <- ifelse(substr(unique(one_context$event), 1, 5) == 'first', 'first', 'second')
  one_context <- add_train_window(one_context, mandate_num)

  min_train_dt <- merge(cases_dt, one_context[, .(location_id, start_date, end_date, onset_date)],
                        by = 'location_id')
  min_train_dt <- min_train_dt[date >= start_date & date <= end_date]
  min_train_dt[, time_id := as.numeric(floor((date - onset_date) / 7))]

  weekly_dt <- min_train_dt[, .(cases  = sum(daily_cases),
                                deaths = sum(daily_deaths)),
                            by = c('location_id', 'time_id')]
  
  # the covariate is defined as the sum over the last 2 weeks
  weekly_dt[, cases_lag2_sum := frollsum(shift(cases, 1), n = 2, align = "right"), by = location_id]
  weekly_dt[, deaths_lag2_sum := frollsum(shift(deaths, 1), n = 2, align = "right"), by = location_id]
  weekly_dt <- weekly_dt[! is.na(cases_lag2_sum)]

  # # of location-weeks with non-zero values must exceed 5
  data.table(
    context_id = context,
    cases = ifelse(sum(weekly_dt$cases_lag2_sum > 0) >5, 1, 0),
    deaths = ifelse(sum(weekly_dt$deaths_lag2_sum > 0) >5, 1, 0)
    #cases  = ifelse(sum(weekly_dt$cases  > 0) / nrow(weekly_dt) > epi_threshold, 1, 0),
    #deaths = ifelse(sum(weekly_dt$deaths > 0) / nrow(weekly_dt) > epi_threshold, 1, 0)
  )
}

epi_covars     <- rbindlist(lapply(unique(context_guide$context_id), check_cases_deaths))
context_lookup <- merge(mandate_covars, epi_covars, by = 'context_id')

# 3(B continued) Document locations with missing cases or deaths data
for (event_name in event_list) {
  one_event   <- all_contexts[event == event_name]
  mandate_num <- ifelse(substr(event_name, 1, 5) == 'first', 'first', 'second')
  one_event_w <- add_train_window(copy(one_event), mandate_num)

  # Locations absent from cases_dt entirely
  absent <- setdiff(one_event_w$location_id, cases_dt$location_id)
  if (length(absent) > 0) {
    drop_log[[paste0(event_name, '__missing_cases_deaths')]] <- data.table(
      location_id = absent, event = event_name, reason = 'missing_cases_deaths_data'
    )
  }

  # For present locations, check for all-NA within the training window
  present_dt <- merge(cases_dt,
                      one_event_w[!location_id %in% absent, .(location_id, start_date, end_date)],
                      by = 'location_id')
  present_dt <- present_dt[date >= start_date & date <= end_date]

  na_cases  <- present_dt[, all(is.na(daily_cases)),  by = location_id][V1 == TRUE, location_id]
  na_deaths <- present_dt[, all(is.na(daily_deaths)), by = location_id][V1 == TRUE, location_id]

  if (length(na_cases) > 0) {
    drop_log[[paste0(event_name, '__na_cases')]] <- data.table(
      location_id = na_cases, event = event_name, reason = 'missing_cases'
    )
  }
  if (length(na_deaths) > 0) {
    drop_log[[paste0(event_name, '__na_deaths')]] <- data.table(
      location_id = na_deaths, event = event_name, reason = 'missing_deaths'
    )
  }
}

# Write drop log
if (length(drop_log) > 0) {
  drop_log_dt <- rbindlist(drop_log)
  fwrite(drop_log_dt, paste0(out_dir, 'dropped_locations_', suffix, '.csv'))
  message(sprintf('%d location-event pair(s) dropped during QC. See dropped_locations_%s.csv.', nrow(drop_log_dt), suffix))
}


# 3(C) Resolve which models to run for each context
context_lookup[, `:=`(
  model_1  = 1,                                                                     # random walk
  model_2  = 1,                                                                     # random walk with trend
  # OLS with lagged outcome
  model_3  = 1,                                                                     # OLS: lagged y
  model_4  = ifelse(cases == 1, 1, 0),                                              # OLS: lagged y + cases
  model_5  = ifelse(deaths == 1, 1, 0),                                             # OLS: lagged y + deaths
  model_6  = ifelse(cases == 1 & deaths == 1, 1, 0),                                # OLS: lagged y + cases + deaths
  model_7  = ifelse(edu == 1, 1, 0),                                                # OLS: lagged y + schools
  model_8  = ifelse(gathering == 1, 1, 0),                                          # OLS: lagged y + gatherings
  model_9  = ifelse(gym == 1, 1, 0),                                                # OLS: lagged y + gym
  model_10  = ifelse(bar == 1, 1, 0),                                               # OLS: lagged y + bar
  model_11 = ifelse(gathering == 1 | bar == 1 | edu == 1 | gym == 1, 1, 0),         # OLS: lagged y + sum of mandates
  # GLS with ARMA(1,1) errors
  model_21 = 1,                                                                     # GLS: 1
  model_22 = ifelse(cases == 1, 1, 0),                                              # GLS: cases
  model_23 = ifelse(deaths == 1, 1, 0),                                             # GLS: deaths
  model_24 = ifelse(cases == 1 & deaths == 1, 1, 0),                                # GLS: cases + deaths
  model_25 = ifelse(edu == 1, 1, 0),                                                # GLS: schools
  model_26 = ifelse(gathering == 1, 1, 0),                                          # GLS: gatherings
  model_27 = ifelse(gym == 1, 1, 0),                                                # GLS: gym
  model_28 = ifelse(bar == 1, 1, 0),                                                # GLS: bar
  model_29 = ifelse(gathering == 1 | bar == 1 | edu == 1 | gym == 1, 1, 0),         # GLS: sum of mandates
  # LME with location random intercepts and ARMA(1,1) errors
  model_30 = 1,                                                                     # LME: (1|location)
  model_31 = ifelse(cases == 1, 1, 0),                                              # LME: cases
  model_32 = ifelse(deaths == 1, 1, 0),                                             # LME: deaths
  model_33 = ifelse(cases == 1 & deaths == 1, 1, 0),                                # LME: cases + deaths
  model_34 = ifelse(edu == 1, 1, 0),                                                # LME: schools
  model_35 = ifelse(gathering == 1, 1, 0),                                          # LME: gatherings
  model_36 = ifelse(gym == 1, 1, 0),                                                # LME: gym
  model_37 = ifelse(bar == 1, 1, 0),                                                # LME: bar
  model_38 = ifelse(gathering == 1 | bar == 1 | edu == 1 | gym == 1, 1, 0),         # LME: sum of mandates
  # Miscellaneous
  model_39 = 0,                                                                     # exponential smoothing
  model_40 = 0                                                                      # neural network
)]


### (4) Final clean up and save to disk ###

# (a) context lookup
cols_to_keep <- c('context_id', 'country', 'ADMN', 'mandate_type', 'mandate_num',
                  'outcome', 'pop_cat', 'pol_cat', 'N', paste0('model_', 1:11), paste0('model_', 21:40))
context_lookup <- context_lookup[, .SD, .SDcols = cols_to_keep]
fwrite(context_lookup, paste0(out_dir, 'context_lookup_', suffix, '.csv'))

# (b) locs by context
loc_map <- all_contexts[, .(context_id, location_id)]
fwrite(loc_map, paste0(out_dir, 'locs_by_context_', suffix, '.csv'))


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
