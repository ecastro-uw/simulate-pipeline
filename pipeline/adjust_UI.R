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
  
  # add observed values
  results_draws <- merge(results_draws, data, by=c('location_id', 'time_id'))
  
  # subset to pre-event period
  dt_summary <- results_draws[time_id < 0]
  
  # summarize
  draw_cols <- paste0('draw_',1:n_draws)
  dt_summary[, `:=`(mean = rowMeans(.SD),
                    LL = apply(.SD, 1, quantile, 0.025),
                    UL = apply(.SD, 1, quantile, 0.975)), .SDcols = draw_cols]
  dt_summary <- dt_summary[, (draw_cols) := NULL]
  
  # Step 2: calculate initial pre-adjustment coverage
  # coverage = % of loc-time steps where the observed value (y) falls within the 95% PI
  coverage_pre <- sum(dt_summary[, ifelse(y>=LL & y<=UL, 1, 0)])/nrow(dt_summary)
  
  # Step 3: Define a function that adjusts the uncertainty interval and calculates the new coverage rate
  calc_adj_coverage <- function(M, dt, by_draw){
    
    dt_copy <- copy(dt)
    
    # A. Adjust
    if(by_draw==F){
      # Inflate distance btwn mean and LL by M to get new LL
      dt_copy[, LL_adj := mean + (LL-mean)*M]
      # Inflate distance btwn mean and UL by M to get new UL
      dt_copy[, UL_adj := mean + (UL-mean)*M]
    } else {
      # Apply the multiplier at draw level
      dt_copy[, mean := rowMeans(.SD), .SDcols = draw_cols]
      draws_adj <- dt_copy[, lapply(.SD, function(x) mean + ((x-mean)*M)), .SDcols = draw_cols]
      draws_adj <- cbind(dt_copy[,.(location_id, time_id, y)], draws_adj)
      
      # Summarize
      dt_copy <- draws_adj[time_id < 0]
      dt_copy[, `:=`(mean = rowMeans(.SD),
                LL_adj = apply(.SD, 1, quantile, 0.025),
                UL_adj = apply(.SD, 1, quantile, 0.975)), .SDcols = draw_cols]
      dt_copy <- dt_copy[, (draw_cols) := NULL]
    }
    
    # B. Calculate the adjusted coverage
    coverage_post <- sum(dt_copy[, ifelse(y>=LL_adj & y<=UL_adj, 1, 0)])/nrow(dt_copy)
    
    # Return results
    if(by_draw==F){
      return(coverage_post)
    } else { 
      # if running by draw, return adjusted draws
      return(list(coverage_post = coverage_post, draws_adj = draws_adj))
    }

  }
  
  adj_and_check <- function(M, dt_summary = dt_summary, target_cov = 0.95){
    
    coverage <- calc_adj_coverage(M, dt_summary, by_draw=F)
    
    # Calc deviation from target coverage
    #val <- (coverage - target_cov)^2
    val <- ((coverage - target_cov)^2)+((M-1)^2/100000)
    return(val)
  }
  
  # Step 4: Find the value of M that minimizes the difference between empirical and target coverage
  M_grid <- seq(0.1, 5, by = 0.01)
  obj_values <- sapply(M_grid, function(m) adj_and_check(m, dt_summary))
  multiplier <- M_grid[which.min(obj_values)]
  
  # Step 5: Apply the multiplier to each draw
  coverage_results <- calc_adj_coverage(M=multiplier, results_draws, by_draw=T)
  coverage_post <- coverage_results$coverage_post
  
  # Step 6: Calculate p-value (what % of draws are less than or equal to the observed value?)
  draws_adj <- coverage_results$draws_adj
  draws_adj$p_val <- rowSums(draws_adj[, lapply(.SD, function(x) x <= y), .SDcols = draw_cols])/n_draws
  
  
  return(list(final_results = draws_adj, 
              multiplier = multiplier, 
              coverage_pre = coverage_pre, 
              coverage_post = coverage_post))
}

