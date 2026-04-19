# Model 17: auto.arima
# Covariates: Gathering restrictions (% of days in effect during the previous week)

model_17 <- function(dataset, w, d) {
  
  dt <- copy(dataset)
  setorder(dt, location_id, time_id)
  
  # Lag gathering restrictions
  dt[, lagged_gathering := shift(pct_gathering), by=location_id]
  
  locations    <- unique(dt$location_id)
  results_list <- vector("list", length(locations))
  
  for (i in seq_along(locations)) {
    loc      <- locations[i]
    loc_data <- dt[location_id == loc]
    setorder(loc_data, time_id)
    
    n              <- nrow(loc_data)
    gathering_observed <- loc_data$pct_gathering
    
    # Remove rows where lagged_gathering is NA (first row due to lagging)
    loc_data_complete <- loc_data[!is.na(lagged_gathering)]
    
    data_ts    <- ts(loc_data_complete$y)
    
    if (length(unique(loc_data_complete$lagged_gathering))==1) {
      # If covariate has no variation, fit without external regressors
      tmp_model <- auto.arima(data_ts)
      
      # Generate d simulation draws for the w-week-ahead forecast.
      # Each simulate() call draws one stochastic path; we keep only step w.
      draws <- sapply(seq_len(d), function(x) {
        sim <- as.numeric(simulate(tmp_model, future = TRUE, nsim = w))
        sim[w]
      })
    } else {
      # Otherwise, fit with external regressors
      xreg_train <- matrix(loc_data_complete$lagged_gathering, ncol = 1,
                           dimnames = list(NULL, "lagged_gathering"))
      tmp_model <- auto.arima(data_ts, xreg = xreg_train)

      # When auto.arima selects a model with drift, it prepends a "drift" column
      # (1:nobs) to xreg internally. We must continue that trend in xreg_future.
      has_drift <- !is.null(tmp_model$xreg) && "drift" %in% colnames(tmp_model$xreg)
      n_train   <- nrow(loc_data_complete)

      # Build future xreg for the w forecast steps.
      # When an index exceeds n, use persistence (last observed value).
      xreg_future <- matrix(NA_real_, nrow = w, ncol = 1,
                            dimnames = list(NULL, "lagged_gathering"))
      for (s in seq_len(w)) {
        idx1 <- n + s - 1
        val1 <- if (idx1 <= n) gathering_observed[idx1] else gathering_observed[n]
        xreg_future[s, 1] <- val1
      }

      if (has_drift) {
        drift_future <- matrix(seq(n_train + 1, n_train + w), nrow = w, ncol = 1,
                               dimnames = list(NULL, "drift"))
        xreg_future <- cbind(drift_future, xreg_future)
      }

      # Generate d simulation draws for the w-week-ahead forecast.
      # Each simulate() call draws one stochastic path; we keep only step w.
      draws <- sapply(seq_len(d), function(x) {
        sim <- as.numeric(simulate(tmp_model, future = TRUE, nsim = w, xreg = xreg_future))
        sim[w]
      })
    } 
    
    # Compute sigma as RMSE of in-sample residuals
    resids <- residuals(tmp_model)
    sigma  <- sqrt(mean(resids^2, na.rm = TRUE))
    
    draws_dt <- as.data.table(t(draws))
    setnames(draws_dt, paste0("draw_", seq_len(d)))
    
    ids <- data.table(
      model       = "model_17",
      location_id = loc,
      time_id     = max(loc_data$time_id) + w,
      sigma       = sigma
    )
    results_list[[i]] <- cbind(ids, draws_dt)
  }
  
  return(rbindlist(results_list))
}
