# Adjust the UI
adjust_UI <- function(data, results_draws, problem_log, configs){
  
  # define some parameters
  w <- configs$w #forecast length
  n_draws <- configs$d #number of draws
  fit_model <- ifelse(length(configs$models)>1, 'ensemble',configs$models)
  
  # generate a list of locations
  loc_list <- unique(results_draws$location_id)
  
  # remove any locations that didnt pass qc
  problem_log$location_id <- as.character(problem_log$location_id)
  loc_list <- setdiff(loc_list, problem_log$location_id)
  
  # Step 1: summarize draws from the pre-event period (mean, LL, UL, observed values)
  
  # subset to pre-event period
  dt_summary <- results_draws[time_id < 0]
  
  # summarize
  draw_cols <- paste0('draw_',1:n_draws)
  dt_summary[, `:=`(mean = rowMeans(.SD),
                    LL = apply(.SD, 1, quantile, 0.025),
                    UL = apply(.SD, 1, quantile, 0.975)), .SDcols = draw_cols]
  dt_summary <- dt_summary[, (draw_cols) := NULL]
  
  # add observed values
  dt_summary <- merge(dt_summary, data, by=c('location_id', 'time_id'))
  dt_summary <- dt_summary[, .(location_id, time_id, obs=y, mean, LL, UL)]
  
  # Step 2: calculate initial pre-adjustment coverage
  coverage_pre <- sum(dt_summary[, ifelse(obs>=LL & obs<=UL, 1, 0)])/nrow(dt_summary)
  
  # Step 3: Define a function that adjusts the uncertainty interval and calculates the new coverage rate
  adj_and_check <- function(I, dt_summary = dt_summary, target_cov = 0.95){
    
    # Inflate distance btwn mean and LL by I
    dt_summary[, d1 := (mean - LL)*I]
    # Inflate distance btwn mean and UL by I
    dt_summary[, d2 := (UL - mean)*I]
    
    # Calc coverage of the new interval
    dt_summary[, verdict := ifelse(obs>=(mean - d1) & obs<=(mean + d2), 1, 0)]
    coverage <- sum(dt_summary$verdict)/nrow(dt_summary)
    
    # Calc deviation from target coverage
    #val <- (coverage - target_cov)^2
    val <- ((coverage - target_cov)^2)+((I-1)^2/100000)
    return(val)
  }
  
  # Step 4: Find the value of I that minimizes the difference between empirical and target coverage
  multiplier <- optimize(f = adj_and_check, 
                         interval = c(0,10),
                         dt_summary)$minimum

  # Step 5: Apply the multiplier to each draw and calculate p-value
  
  # apply multiplier
  results_draws[, mean := rowMeans(.SD), .SDcols = draw_cols]
  draws_adj <- results_draws[, lapply(.SD, function(x) mean + ((x-mean)*multiplier)), .SDcols = draw_cols]
  draws_adj <- cbind(results_draws[,.(location_id, time_id)], draws_adj)
  
  # calculate p-value (what % of draws are less than or equal to the observed value?)
  draws_adj <- merge(draws_adj, data, by=c('location_id', 'time_id'))
  draws_adj$p_val <- rowSums(draws_adj[, lapply(.SD, function(x) x <= y), .SDcols = draw_cols])/n_draws
  
  # Step 6: Calculate post-adjustment coverage 
  dt_summary_post <- draws_adj[time_id < 0]
  dt_summary_post[, `:=`(mean = rowMeans(.SD),
                         LL = apply(.SD, 1, quantile, 0.025),
                         UL = apply(.SD, 1, quantile, 0.975)), .SDcols = draw_cols]
  dt_summary_post <- dt_summary_post[, (draw_cols) := NULL]
  coverage_post <- sum(dt_summary_post[, ifelse(y>=LL & y<=UL, 1, 0)])/nrow(dt_summary_post)
  
  return(list(final_results = draws_adj, 
              multiplier = multiplier, 
              coverage_pre = coverage_pre, 
              coverage_post = coverage_post))
}

