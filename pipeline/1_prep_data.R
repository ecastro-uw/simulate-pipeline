# 1_prep_data
# return a data.table with columns: location_id, time_id, y, cov1, cov2, etc.
# if the file already exists, load it in. otherwise, create it.

prep_data <- function(pipeline_inputs){
  
  input_dir <- pipeline_inputs$input_dir
  out_dir <- pipeline_inputs$out_dir
  configs <- pipeline_inputs$configs
  
  # which country?
  country <- configs$country
  
  # what is the geographic unit of analysis?
  loc_type <- configs$location_type
  
  # which locations?
  location_list <- pipeline_inputs$loc_list
  
  # which mandate?
  mandate <- configs$mandates
  
  # which imposition?
  imposition <- configs$imposition
  
  # which mobility data?
  data_source <- configs$data_source

  print(paste(country, loc_type, '-', imposition, mandate, 'mandate -', data_source))
  input_subdir <- paste0(input_dir,country,'_', loc_type, '/')
  
  # Ready to assemble the data set
  
  ### (1) First, pull in timing data for event of interest
  event_dt <- fread(paste0(input_subdir, imposition,'_',mandate,'_close.csv'))[location_id %in% location_list]
  if(nrow(event_dt)==0){
    stop(paste("Location set is incompatible with desired mandate & imposition: these locations did not impose",
               imposition, mandate,"mandates."))
  }
  missing_event_data <- setdiff(location_list, event_dt$location_id)
  
  ### (2) Second, load and process the outcome data
  if (data_source=='safegraph'){
    
    # Which data to grab?
    cat_name <- ifelse(mandate=='restaurant', 'Restaurants', 'Drinking')
    var_name <- 'visits_per_10k'
    
    # Load the data
    outcome_dt <- fread(paste0(input_subdir,'/processed_safegraph_data.csv'))[location_id %in%
                   location_list & top_category %like% cat_name, .(location_id, date, v = get(var_name))]
    setorder(outcome_dt, location_id, date)
    
    # Calculate baseline for normalization (use same period as Google)
    baseline_dt <- outcome_dt[date >= '2020-01-03' & date <= '2020-02-06'] 
    # subset to locations with no missing data during the 91-day baseline period
    complete_baseline <- baseline_dt[!is.na(v), .N, by = location_id][N == 35, location_id]
    baseline_dt <- baseline_dt[location_id %in% complete_baseline]
    # aggregate to the week level
    baseline_dt[, week_id := rep(1:5, each = 7), by = location_id]
    baseline_weekly <- baseline_dt[, .(v = sum(v)), by = c('location_id', 'week_id')]
    # calculate the mean value over the baseline period for each location
    baseline <- baseline_weekly[, .(mean_base = mean(v)), by = location_id]
    incomplete_baseline <- setdiff(location_list, complete_baseline)

    
    # Get data for the desired study window
    # If first mandate, 8 weeks prior (default train weeks) and 1 week post (w)
    # If second mandate, all but 2 (padding) weeks between the mandates and 1 week post (w)
    if (imposition=='first'){
      # Subset to appropriate window
      outcome_dt <- merge(outcome_dt, event_dt, by='location_id')
      outcome_dt <- outcome_dt[date >= (onset_date - (configs$default_train_wks*7)) &
                                date < (onset_date + (configs$w*7))]
      
    } else{
      # Subset to appropriate window
      outcome_dt <- merge(outcome_dt, event_dt, by='location_id')
      
      outcome_dt[, start_date := {
        base_date <- prev_lift + configs$padding*7
        onset_dow  <- wday(onset_date)   # day of week for onset_date (1=Sun … 7=Sat)
        base_dow   <- wday(base_date)    # day of week for base_date
        days_to_add <- (onset_dow - base_dow) %% 7  # 0–6 additional days needed
        base_date + days_to_add
      }]
      outcome_dt <- outcome_dt[date >= start_date & date < (onset_date + (configs$w*7))]
      
    }
    
    # Add week id
    outcome_dt[, time_id := as.numeric(floor((date - onset_date) / 7)), by = location_id]
    
    # Check for missingness
    if (imposition=='first'){
      # Check if any locations having missing weeks (we expect exactly 9 weeks for first imposition)
      missing_weeks <- outcome_dt[, length(unique(time_id)), by=location_id][V1 < (configs$default_train_wks + configs$w)]
      
      # Check if any locations have missing days (each time_id should have 7 rows associated with it)
      missing_days <- outcome_dt[, .N, by=c('location_id', 'time_id')][N!=7]
      
      missing_outcome_data <- unique(c(missing_weeks$location_id, missing_days$location_id))
    } else {
      # Check if the onset date extends beyond the available data
      onset_after_data_ends <- outcome_dt[, max(time_id), by=location_id][V1<0]$location_id
      missing_outcome_data <- onset_after_data_ends
    }
    
    # Drop any locations with missing data during the study period
    outcome_dt <- outcome_dt[! location_id %in% missing_outcome_data]
    
  } else{
    # Google
    #TODO
  }
  
  # Output a log of missing locs with reasons for missingness
  problem_log <- data.table(location_id = missing_event_data,
                            reason = rep("Missing mandate data", length(missing_event_data))
                            )
  if(imposition=='first'){
    rows <- data.table(location_id = c(missing_days, missing_weeks),
                       reason = c(rep("Missing days of mobility data", length(missing_days)),
                                  rep("Missing weeks of mobility data"), length(missing_weeks)))
  } else {
    rows <- data.table(location_id = onset_after_data_ends,
                       reason = rep("Mandate onset is after end of mobility data", length(onset_after_data_ends)))
  }
  problem_log <- rbind(problem_log, rows)
  
  if(data_source=='safegraph'){
    rows <- data.table(location_id=incomplete_baseline,
                       reason=rep('Incomplete baseline data', length(incomplete_baseline)))
    problem_log <- rbind(problem_log, rows)
  }
  
  
  ### (3) Finally, add covariate data
  ## 3(a) Other mandates
  mandate_dt <- fread(paste0(input_subdir,'other_mandate_time_series.csv'))[location_id %in% location_list]
  outcome_dt <- merge(outcome_dt, mandate_dt, by=c('location_id','date'), all.x=T)
  
  ## 3(b) Covid cases and deaths (per 10K pop)
  covid_dt <- fread(paste0(input_subdir,'covid_cases_deaths.csv'))[location_id %in% location_list]
  pop_dt   <- fread(paste0(input_subdir,'population.csv'))[location_id %in% location_list]
  covid_dt <- merge(covid_dt, pop_dt, by='location_id', all.x=T)
  outcome_dt <- merge(outcome_dt,
                      covid_dt[, .(location_id, date, daily_cases, daily_deaths, pop)],
                      by=c('location_id', 'date'), all.x=T)
  
  # Check for missingness 
  covar_list <- c('primary_edu','gatherings50i100o','gym_pool_leisure_close',
                  'non_essential_retail_close', 'stay_at_home', 'dining_close', 'bar_close',
                  'daily_cases', 'daily_deaths')
  missing_covars <- data.table(location_id=integer(), covariate=character())
  for(covar in covar_list){
    temp <- data.table(location_id = unique(outcome_dt[is.na(get(covar)),location_id]),
                       covariate = covar)
    missing_covars <- rbind(missing_covars, temp)
  }
  for(loc in unique(missing_covars$location_id)){
    one_row <- data.table(
      location_id = loc,
      reason = paste('Missing covariate data: ', paste(missing_covars[location_id==loc,covariate], collapse=', '))
    )
    problem_log <- rbind(problem_log, one_row)
  }
  # Drop locations with missing covariate data
  outcome_dt <- outcome_dt[! location_id %in% unique(missing_covars$location_id)]
  
  
  ### (4) Summarize to weekly level
  weekly_dt <- outcome_dt[, .(v = sum(v),
                              pct_edu = sum(primary_edu)/7,
                              pct_gathering = sum(gatherings50i100o)/7,
                              pct_gym = sum(gym_pool_leisure_close)/7,
                              pct_retail = sum(non_essential_retail_close)/7,
                              pct_sah = sum(stay_at_home)/7,
                              pct_dining = sum(dining_close)/7,
                              pct_bar = sum(bar_close)/7,
                              cases_pc = sum(daily_cases)/unique(pop)*10000,
                              deaths_pc = sum(daily_deaths)/unique(pop)*10000),
                          by = c('location_id', 'time_id')]
  
  # Perform final processing in weekly space
  if(data_source=='safegraph'){

    # Normalize by 2019 baseline then convert to log space
    weekly_dt <- merge(weekly_dt, baseline, by = 'location_id')
    dt <- weekly_dt[, y := log(v / mean_base)] #TODO - how to handle instances when v=0? results in y=-Inf
    
    dt <- dt[, .(location_id, time_id, y, cases_pc, deaths_pc,
                 pct_edu, pct_gathering, pct_gym, pct_retail,
                 pct_sah, pct_dining, pct_bar)]
  }
  
  # Save out the problem log
  if(nrow(problem_log)>0){
    fwrite(problem_log, paste0(out_dir,'/logs/dropped_locs_context_', pipeline_inputs$context_id,'.csv'))
  }
  
  # Ensure dt is properly sorted
  setorder(dt, location_id, time_id)

  # Return the dataset
  return(dt)
}
