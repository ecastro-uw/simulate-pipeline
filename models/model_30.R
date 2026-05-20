# Model 30: Exponential Smoothing Model
# Covariates: None

model_30 <- function(dataset, w, d, use_param_uncertainty = TRUE){
  
  # Description of function:
  # Fit the model on the dataset provided and output d draws of the w-week-ahead forecast.
  # The exponential smoothing model predicts future values by assigning exponentially decreasing
  # weights to past data. Fit a separate model for each location with automatic selection of 
  # optimal ETS(E,T,S) family via AIC and BIC.
  
  # Make a copy so the original stays unchanged
  dt <- copy(dataset)
  dt <- dt[!is.na(y)]
  
  locations <- unique(dt$location_id)
  results_list <- vector("list", length(locations))
  
  # Fit one model per location
  for (i in seq_along(locations)){
    
    # subset and sort data for current location
    loc <- locations[i]
    loc_data <- dt[location_id == loc]
    setorder(loc_data, time_id)
    
    # define time series
    data_ts <- ts(loc_data$y)
    
    # Fit the ETS with automatic model selection
    # restrict = F allows multiplicative error models
    tmp_model <- ets(data_ts, restrict = FALSE)
    
    # Compute sigma as the RMSE of in-sample resids
    resids <- residuals(tmp_model)
    sigma  <- sqrt(mean(resids^2, na.rm=T))

    # Generate d draws for the w-week-ahead forecast.
    # With parameter uncertainty: stochastic simulation paths from the fitted model.
    # Without parameter uncertainty: point forecast plus residual noise only.
    if (use_param_uncertainty) {
      draws <- sapply(seq_len(d), function(x) {
        sim <- as.numeric(simulate(tmp_model, future = TRUE, nsim = w))
        sim[w]
      })
    } else {
      pt_forecast <- as.numeric(forecast(tmp_model, h = w)$mean)[w]
      draws <- rnorm(d, mean = pt_forecast, sd = sigma)
    }
    
    # Compile the forecasts
    draws_dt <- as.data.table(t(draws))
    setnames(draws_dt, paste0("draw_", seq_len(d)))
    
    ids <- data.table(
      model       = "model_30",
      location_id = loc,
      time_id     = max(loc_data$time_id) + w,
      sigma       = sigma
    )
    results_list[[i]] <- cbind(ids, draws_dt)
  }
  
  return(rbindlist(results_list))
}