# Adjust the UI
adjust_UI <- function(results_draws, problem_log, configs){
  
  # define forecast length
  w <- configs$w
  
  # define number of draws
  n_draws <- configs$d
  
  # generate a table of mandate impositions for each county
  loc_list <- names(results_draws)
  all_events <- rbindlist(lapply(loc_list, function(x) data.table(location_id=x, instance=1:length(results_draws[[x]]))))
  
  # remove any instances that didnt pass qc
  problem_log$location_id <- as.character(problem_log$location_id)
  all_events <- all_events[! problem_log, on=.(location_id=location_id, instance)]
  
  # Step 1: create a dt from the results object with relevant information from the 
  # pre-mandate period (log mean visits, log LL, log UL, log observed visits)
  build_dt <- function(row_num, all_events, results_draws){
  for(row_num in 1:nrow(all_events)){
    loc <- all_events[row_num]$location_id
    i <- all_events[row_num]$instance
    dt_temp <- results_draws[[loc]][[i]]
    
    # calculate mean, ll, ul from ensemble draws (in log space)
    draws_only <- log(dt_temp$ensemble[names(dt_temp$ensemble) %like% 'draw'])
    dt_partial <- data.table(time_id=unique(dt_temp$ensemble$time_id),
                       log_mean = rowMeans(draws_only),
                       log_ll = apply(draws_only,1,quantile,0.025),
                       log_ul = apply(draws_only,1,quantile,0.975))
      
    # add observed values
    dt_partial <- merge(dt_partial, dt_temp$obs, by='time_id', all.x=T)
    dt_partial <- dt_partial[mand_on==0, .(location_id=loc, time_id, obs=y,
                    log_obs = log(y), log_mean, log_ll, log_ul)]
  }  
    return(dt_partial)
  }
  dt_summary <- rbindlist(lapply(1:nrow(all_events), build_dt, all_events, results_draws))
  
  
  # Step 2: Define a function that adjusts the uncertainty interval and calculates the new coverage rate
  adj_and_check <- function(I, dt_summary, target_cov = 0.95){
    
    # Inflate distance btwn mean and LL by I
    dt_summary[, d1 := (log_mean - log_ll)*I]
    # Inflate distance btwn mean and UL by I
    dt_summary[, d2 := (log_ul - log_mean)*I]
    
    # Calc coverage of the new interval
    dt_summary[, verdict := ifelse(log_obs>=(log_mean - d1) & log_obs<=(log_mean + d2), 1, 0)]
    coverage <- sum(dt_summary$verdict)/nrow(dt_summary)
    
    # Calc deviation from target coverage
    diff <- abs(coverage - target_cov)
    return(diff)
  }
  
  # Step 3: Find the value of I that minimizes "diff" (the difference between empirical and target coverage)
  multiplier <- optimize(f = adj_and_check, 
                         interval = c(0,10),
                         dt_summary)$minimum

  # Step 4: Apply the multiplier to each draw in the post-mandate period and calculate p-value
  #apply_multiplier <- function(row_num, multiplier, results_draws){
  for(row_num in 1:nrow(all_events)){  
    loc <- all_events[row_num]$location_id
    i <- all_events[row_num]$instance
    dt_temp <- results_draws[[loc]][[i]]$ensemble
    
    # make the adjustment (in log space)
    dt_log <- log(dt_temp[names(dt_temp) %like% 'draw'])
    dt_log$log_mean <- rowMeans(dt_log)
    dt_log <- as.data.table(dt_log)
    log_adj_draws <- dt_log[, lapply(.SD, function(x) log_mean + ((x-log_mean)*multiplier)), .SDcols = paste0('draw_',1:n_draws)]
    adj_draws <- cbind(time_id=dt_temp$time_id, exp(log_adj_draws))
    
    # add to results
    results_draws[[loc]][[i]]$ensemble_adj <- adj_draws
    
    # calculate p-value (what % of draws are less than or equal to the observed value?)
    obs_temp <- results_draws[[loc]][[i]]$obs
    adj_draws <- merge(adj_draws, obs_temp[,.(time_id, obs=y)], by='time_id')
    p_vals <- rowSums(adj_draws[, lapply(.SD, function(x) (x - obs)<=0), .SDcols = paste0('draw_',1:n_draws)])/n_draws
    p_vals_dt <- data.table(time_id = dt_temp$time_id, p = p_vals)
    
    # add to results
    results_draws[[loc]][[i]]$p_values <- p_vals_dt
    
    #return(results_draws)
  }
  #test <- lapply(1:nrow(all_events), apply_multiplier, multiplier, results_draws)
  
  return(results_draws)
  
}

