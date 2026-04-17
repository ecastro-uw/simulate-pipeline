# Adjust the UI
adjust_UI <- function(data, results_draws, configs){
  
  # define some parameters
  n_draws <- configs$d #number of draws
  
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
  
  # Step 4: Find the value of M using bisection method
  bisect_for_coverage <- function(dt, target_cov = 0.95, max_iter = 40) {

    # Initialize bounds
    M_low <- 0
    M_high <- 50

    # Get coverage at bounds
    cov_low <- calc_adj_coverage(M_low, dt, by_draw = FALSE)
    cov_high <- calc_adj_coverage(M_high, dt, by_draw = FALSE)

    diff <- cov_high - cov_low
    iter <- 0
    
    cov_dt <- data.table()
    # Bisection loop
    while (iter < max_iter){
      # continue bisecting until reach max iter
      M_mid <- (M_low + M_high) / 2
      cov_mid <- calc_adj_coverage(M_mid, dt, by_draw = FALSE)
    
      one_row <- data.table(iter=iter, low = M_low, high = M_high, mid = M_mid, coverage = cov_mid)
      cov_dt <- rbind(cov_dt, one_row)
      
      # Update bounds
      if (cov_mid < target_cov) {
        M_low <- M_mid
        cov_low <- cov_mid
      } else {
        M_high <- M_mid
        cov_high <- cov_mid
      }
      diff <- cov_high - cov_low
      iter <- iter + 1
    }

    # Return M high 
    return(M_high)
  }

  multiplier <- bisect_for_coverage(dt_summary, target_cov = 0.95)

  # Step 4b: Find second multiplier calibrated on time_id < -1
  dt_summary2 <- results_draws[time_id < -1]
  dt_summary2[, `:=`(mean = rowMeans(.SD),
                     LL = apply(.SD, 1, quantile, 0.025),
                     UL = apply(.SD, 1, quantile, 0.975)), .SDcols = draw_cols]
  dt_summary2 <- dt_summary2[, (draw_cols) := NULL]
  multiplier2 <- bisect_for_coverage(dt_summary2, target_cov = 0.95)

  # Step 5: Apply the multiplier to each draw
  coverage_results <- calc_adj_coverage(M=multiplier, results_draws, by_draw=T)
  coverage_post <- coverage_results$coverage_post

  # Step 6: Calculate p-value (what % of draws are less than or equal to the observed value?)
  draws_adj <- coverage_results$draws_adj
  draws_adj$p_val <- rowSums(draws_adj[, lapply(.SD, function(x) x <= y), .SDcols = draw_cols])/n_draws

  # Step 6b: Apply multiplier2 to draws at time_id = -1, summarize to quantiles
  dt_m1 <- results_draws[time_id == -1]
  dt_m1[, mean := rowMeans(.SD), .SDcols = draw_cols]
  draws_adj2 <- dt_m1[, lapply(.SD, function(x) mean + ((x - mean) * multiplier2)), .SDcols = draw_cols]
  draws_adj2 <- cbind(dt_m1[, .(location_id, time_id)], draws_adj2)

  pi_probs <- c(0.025, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45,
                0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975)
  pi_names <- paste0('q', pi_probs * 100)
  quantile_dt <- as.data.table(t(apply(draws_adj2[, .SD, .SDcols = draw_cols], 1, quantile, pi_probs)))
  setnames(quantile_dt, pi_names)
  final_results2 <- cbind(draws_adj2[, .(location_id, time_id)], quantile_dt)

  return(list(final_results = draws_adj,
              multiplier = multiplier,
              coverage_pre = coverage_pre,
              coverage_post = coverage_post,
              multiplier2 = multiplier2,
              final_results2 = final_results2))
}

