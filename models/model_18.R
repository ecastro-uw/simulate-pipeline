# Model 18: auto.arima
# Covariates: Gym closures (% of days in effect during the previous week)

model_18 <- function(dataset, w, d) {
  
  dt <- copy(dataset)
  setorder(dt, location_id, time_id)
  
  # Lag gym closures
  dt[, lagged_gym := shift(pct_gym), by=location_id]
  
  locations    <- unique(dt$location_id)
  results_list <- vector("list", length(locations))
  
  for (i in seq_along(locations)) {
    loc      <- locations[i]
    loc_data <- dt[location_id == loc]
    setorder(loc_data, time_id)
    
    n              <- nrow(loc_data)
    gym_observed <- loc_data$pct_gym
    
    # Remove rows where lagged_gym is NA (first row due to lagging)
    loc_data_complete <- loc_data[!is.na(lagged_gym)]
    
    data_ts    <- ts(loc_data_complete$y)
    
    if (sum(loc_data_complete$lagged_gym)>0) {
      # If the covariate data are not all 0s, fit with external regressors
      xreg_train <- matrix(loc_data_complete$lagged_gym, ncol = 1,
                           dimnames = list(NULL, "lagged_gym"))
      tmp_model <- auto.arima(data_ts, xreg = xreg_train)
      
      # Build future xreg for the w forecast steps.
      # When an index exceeds n, use persistence (last observed value).
      xreg_future <- matrix(NA_real_, nrow = w, ncol = 1,
                            dimnames = list(NULL, "lagged_gym"))
      for (s in seq_len(w)) {
        idx1 <- n + s - 1
        val1 <- if (idx1 <= n) gym_observed[idx1] else gym_observed[n]
        xreg_future[s, 1] <- val1
      }
      
      # Generate d simulation draws for the w-week-ahead forecast.
      # Each simulate() call draws one stochastic path; we keep only step w.
      draws <- sapply(seq_len(d), function(x) {
        sim <- as.numeric(simulate(tmp_model, future = TRUE, nsim = w, xreg = xreg_future))
        sim[w]
      })
    } else {
      # Otherwise fit without them
      tmp_model <- auto.arima(data_ts)
      
      # Generate d simulation draws for the w-week-ahead forecast.
      # Each simulate() call draws one stochastic path; we keep only step w.
      draws <- sapply(seq_len(d), function(x) {
        sim <- as.numeric(simulate(tmp_model, future = TRUE, nsim = w))
        sim[w]
      })
    }
    
    # Compute sigma as RMSE of in-sample residuals
    resids <- residuals(tmp_model)
    sigma  <- sqrt(mean(resids^2, na.rm = TRUE))
    
    draws_dt <- as.data.table(t(draws))
    setnames(draws_dt, paste0("draw_", seq_len(d)))
    
    ids <- data.table(
      model       = "model_18",
      location_id = loc,
      time_id     = max(loc_data$time_id) + w,
      sigma       = sigma
    )
    results_list[[i]] <- cbind(ids, draws_dt)
  }
  
  return(rbindlist(results_list))
}
