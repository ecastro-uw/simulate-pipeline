# Adjust the UI
adjust_UI <- function(results_draws, problem_log, configs){
  
  # define some parameters
  w <- configs$w #forecast length
  n_draws <- configs$d #number of draws
  fit_model <- ifelse(length(configs$models)>1, 'ensemble',configs$models)
  
  # generate a list of locations
  loc_list <- names(results_draws)
  
  # remove any locations that didnt pass qc
  problem_log$location_id <- as.character(problem_log$location_id)
  loc_list <- setdiff(loc_list, problem_log$location_id)
  
  # Step 1: create a dt from the results object with relevant information from the 
  # pre-mandate period (log mean visits, log LL, log UL, log observed visits)
  build_dt <- function(loc_id, results_draws){
    
    # grab the observations and fitted values
    dt_temp <- results_draws[[loc_id]][[fit_model]]
    dt_obs <- results_draws[[loc_id]][['obs']]
    
    # calculate mean, ll, ul of model draws (in log space)
    draw_cols <- paste0('draw_',1:n_draws)
    log_draws <- log(dt_temp[, ..draw_cols])
    dt_partial <- data.table(time_id=unique(dt_temp$time_id),
                       log_mean = rowMeans(log_draws),
                       log_ll = apply(log_draws,1,quantile,0.025),
                       log_ul = apply(log_draws,1,quantile,0.975))
      
    # add observed values to the table
    dt_partial <- merge(dt_partial, dt_obs, by='time_id', all.x=T)
    dt_partial <- dt_partial[time_id < 0, .(location_id=loc_id, time_id, obs=y,
                    log_obs = log(y), log_mean, log_ll, log_ul)]
    
    return(dt_partial)
  }
  
  dt_summary <- rbindlist(lapply(loc_list, build_dt, results_draws))
  
  
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
  for (loc_id in loc_list){
    
    # grab the observations and fitted values
    dt_temp <- results_draws[[loc_id]][[fit_model]]
    dt_obs <- results_draws[[loc_id]][['obs']]
    
    # make the adjustment (in log space)
    draw_cols <- paste0('draw_',1:n_draws)
    log_draws <- log(dt_temp[, ..draw_cols])
    log_draws$mean <- rowMeans(log_draws)
    log_draws <- as.data.table(log_draws)
    log_draws_adj <- log_draws[, lapply(.SD, function(x) mean + ((x-mean)*multiplier)), .SDcols = paste0('draw_',1:n_draws)]
    draws_adj <- cbind(time_id=dt_temp$time_id, exp(log_draws_adj))
    
    # add to results
    results_draws[[loc_id]]$ensemble_adj <- draws_adj
    
    # calculate p-value (what % of draws are less than or equal to the observed value?)
    draws_adj <- merge(draws_adj, dt_obs[,.(time_id, obs=y)], by='time_id')
    p_vals <- rowSums(draws_adj[, lapply(.SD, function(x) x <= obs), .SDcols = paste0('draw_',1:n_draws)])/n_draws
    p_vals_dt <- data.table(time_id = dt_temp$time_id, p = p_vals)
    
    # add to results
    results_draws[[loc_id]]$p_values <- p_vals_dt
  }
  
  return(results_draws)
}

