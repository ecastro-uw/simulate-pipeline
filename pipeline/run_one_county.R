# Run One County

# Description: For a single county, enumerate all instances of the specified mandate type (e.g. restaurant closures). For each instance passing QC,
# invoke the ensemble_and_forecast function.

run_one_county <- function(root_dir, loc_id, mandate_type, configs, hierarchy){
  
  # Define data paths
  input_data_path <- paste0(root_dir, 'intermediate/visits_by_cnty_day_cat_', configs$input_data_version,'.csv')
  data_requirements_path <- paste0(root_dir, 'inputs/config_files/min_data_requirement_by_model.csv')
  
  # Define name of the mandate variable according to mandate type
  var_name <- ifelse(mandate_type == 'Restaurants', 'dining_close', 'bar_close')
  
  # Define dependent variable
  dep_var <- ifelse(configs$dep_var=='per capita', 'visits_per_10k', 'visit_count')
  
  ## Load data (mobility and mandates) for the county
  county_dt <- fread(input_data_path)[location_id==loc_id & top_category %like% mandate_type,
                                      .(location_id, fips, county, date = as.Date(date),
                                      y=get(dep_var), mandate = get(var_name))]
  
  # Check if this location has any mobility data
  if(nrow(county_dt)==0){
    # No data for this location
    
    # Empty results object
    county_draws <- list(n1=NULL)
    
    # Report in problem log
    county_problem_log <- data.table(location_id=loc_id, instance=1, onset=as.Date(NA),
                          issue="No mobility data",
                          details=paste("There is no mobility data available for",
                                        hierarchy[location_id==loc_id, location_name]))
    
  } else if(nrow(county_dt[y==0])/nrow(county_dt) > 0.3) {
    # more than 30% of rows in the dataset have 0 visits
    # not enough data to proceed
    #NOTE: In the future, 0s may only be problematic in some scenarios. Will need to make this
    # check more specific.
    # Empty results object
    county_draws <- list(n1=NULL)
    
    # Report in problem log
    county_problem_log <- data.table(location_id=loc_id, instance=1, onset=as.Date(NA),
                                     issue="Zero inflated",
                                     details=paste("More than 30% of days exhibit 0 visits in",
                                                   hierarchy[location_id==loc_id, location_name]))
  } else if(configs$dep_var=='per capita' & is.na(sum(county_dt$y))){
    # fitting the models in per capita space but the location is missing population data
    county_draws <- list(n1=NULL)
    
    county_problem_log <- data.table(location_id=loc_id, instance=1, onset=as.Date(NA),
                                     issue="No population data",
                                     details=paste("There is no population data available for", hierarchy[location_id==loc_id, location_name]))
    
  } else if(sum(county_dt$mandate)==0){
    # mandate was never imposed. cant proceed.
    # Empty results object
    county_draws <- list(n1=NULL)
    
    # Report in problem log
    county_problem_log <- data.table(location_id=loc_id, instance=1, onset=as.Date(NA),
                                     issue="No mandate",
                                     details=paste("The", tolower(mandate_type), "mandate was not imposed in",
                                                   hierarchy[location_id==loc_id, location_name]))
  } else {
    ## Create a list of all mandate impositions (of the specified mandate_type) occurring in the county
    events_dt <- county_dt[,.(location_id, date, mandate)][order(location_id, date)]
    events_dt[, lag := shift(mandate, fill = 0), by=location_id]
    events_dt <- events_dt[mandate != lag]
    events_dt <- events_dt[, `:=` (lift = shift(date, type="lead"),
                                   prev_lift = shift(date, type="lag"))][mandate==1]
    events_dt[is.na(lift), lift := max(county_dt$date)] #if mandate is still in effect when data ends, set lift date to last day of data
    events_dt[, num := seq_len(.N), by = location_id]
    events_dt <- events_dt[, .(num, location_id, onset = date, lift, prev_lift)]
    
    ## Determine how many weeks of data to use for training the models
    
    # The minimum number of weeks of data is determined by the list of models and their minimum data requirements for
    # making forecasts with uncertainty. The minimum number of data points required by the most data-intensive
    # model will set the minimum for all models.
    data_req_dt <- fread(data_requirements_path)
    models <- configs$models
    events_dt$min_train_t <- max(data_req_dt[weeks_ahead==configs$w, ..models])
    
    # The maximum number of weeks of data available for training the model is determined by the number of weeks 
    # preceding mandate implementation during which the pandemic is in progress but no previous iteration of the mandate 
    # is in place. Padding refers to the number of weeks after a mandate has been lifted before the mobility data may be used for 
    # training a no-mandate behavior model.
    #NOTE: this line assumes data are daily. perhaps update this to account for the possibility of weekly data
    events_dt[, max_train_t := ifelse(num==1, configs$default_train_wks, floor((as.numeric(onset - prev_lift)/7)-configs$padding))]
    
    ## Perform the ITS analysis for each event
    
    # Create a list for storing county results and a problem log for storing any issue reports for this county
    county_draws <- vector(mode="list", length=nrow(events_dt))
    names(county_draws) <- paste0('n',events_dt$num)
    county_problem_log <- data.table()
    
    
    # If QC is passed, invoke the ensemble_and_forecast() function to conduct the ITS analysis where the event of interest
    # is mandate implementation. If QC fails, make a record in the problem log.
    for (i in 1:nrow(events_dt)){
      one_row <- events_dt[i]
      min_train_t <- one_row$min_train_t
      max_train_t <- one_row$max_train_t
      
      if(min_train_t + configs$w > max_train_t){
      # 1. Check that the available training weeks meet the minimum data requirements for all models.
        result <- data.table(location_id=one_row$location_id, instance=one_row$num, onset=one_row$onset,
                             issue="Not enough training data",
                             details=paste0("The models require ", eval(min_train_t + configs$w), " weeks of data (", min_train_t,
                                            " weeks for fitting and ", configs$w, " weeks for calculating residuals for the ", configs$w,
                                            "-week-ahead forecast) but only ", max_train_t, " weeks of data are available."))
      } else if(one_row$onset + configs$w*7 > one_row$lift){ #NOTE - this assumes daily data
      # 2. Check the mandate lift date to ensure that the w-week ahead forecast doesn't extend beyond the mandate period.
        result <- data.table(location_id=one_row$location_id, instance=one_row$num, onset=one_row$onset,
                             issue="Forecast period exceeds mandate",
                             details=paste("The forecast period of", configs$w, "week(s) extends beyond the mandate's duration of",
                                           floor(as.numeric(one_row$lift - one_row$onset)/7), "weeks."))
      #} else if(one_row$lift + configs$w*7 > max(county_dt$date)){ #note: removed on 4/20/25, the check is immaterial
      #  # 3. Check that the forecast period doesn't extend beyond the data
      #  result <- data.table(location_id=one_row$location_id, instance=one_row$num, onset=one_row$onset,
      #                       issue="Mandate period extends beyond mobility data",
      #                       details=paste("The mandate imposed on", one_row$onset, "was still in effect on", max(county_dt$date), 
      #                                     "(the last available day of mobility data)."))
      } else if(one_row$onset + configs$w*7 > max(county_dt$date)){   # note: added on 4/20/25 // assumes daily data
        # 4. Check that the forecast period doesn't extend beyond the data
        result <- data.table(location_id=one_row$location_id, instance=one_row$num, onset=one_row$onset,
                             issue="Forecast period exceeds data",
                             details=paste("The forecast period of", configs$w, "week(s) extends beyond", max(county_dt$date), 
                                           "(the last available day of mobility data)."))
      #} else if(one_row$num!=2){ #TEMP FOR TESTING
      #  result <- data.table(location_id=one_row$location_id, instance=one_row$num, onset=one_row$onset,
      #                       issue="Not a second imposition",
      #                       details=paste("Test run for second impositions only"))
      } else {
        # ALL QC CHECKS PASSED. CAN PROCEED TO ANALYSIS.
        
        # But first, aggregate data to the week level
        dt <- daily_to_weekly(one_row, county_dt, configs)
        
        result <- ensemble_and_forecast(county_dt = dt,
                                        configs, min_train_t, max_train_t)
      }
      
      # If the mandate imposition DID NOT pass QC, note it in the problem log
      # If the mandate imposition DID pass QC, add model forecasts to the list of results
      if ("issue" %in% names(result)){
        county_problem_log <- rbind(county_problem_log, result)
      } else { 
        # otherwise, save the draws to the list of draws
        county_draws[[paste0('n', one_row$num)]] <- result
      }
      
    } #end loop through event rows
  } #end elseif for locs w/ data
  
  county_results <- list(results = county_draws, problem_log = county_problem_log)
  return(county_results)
} 
