# Model 39: Exponential Smoothing Model
# Covariates: None

model_39 <- function(dataset, w, d){
  
  # Description of function:
  # Fit the model on the dataset provided and output d draws of the w-week-ahead forecast.
  # The exponential smoothing model predicts future values by assigning exponentially decreasing
  # weights to past data. Fit a separate model for each location with automatic selection of 
  # optimal ETS(E,T,S) family via AIC and BIC.
  
  # Make a copy so the original stays unchanged
  dt <- copy(dataset)
  
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
    
    # Generate d simulation draws for the w-week-ahead forecast
    draws <- sapply(seq_len(d), function(x) {
      sim <- as.numeric(simulate(tmp_model, future = T, nsim = w))
      sim[w]
    })
    
    # Compute sigma as the RMSE of in-sample resids
    resids <- residuals(tmp_model)
    sigma  <- sqrt(mean(resids^2, na.rm=T)) 
    
    # Compile the forecasts
    draws_dt <- as.data.table(t(draws))
    setnames(draws_dt, paste0("draw_", seq_len(d)))
    
    ids <- data.table(
      model       = "model_39",
      location_id = loc,
      time_id     = max(loc_data$time_id) + w,
      sigma       = sigma
    )
    results_list[[i]] <- cbind(ids, draws_dt)
  }
  
  return(rbindlist(results_list))
}