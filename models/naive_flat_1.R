
naive_flat_1 <- function(dataset, w, d){
  
  # Description of function:
  # Fit the model on the dataset provided and output d draws of the w-week-ahead forecast.
  # The naive flat 1 model takes the last observation and propagates it w weeks into the future.
  
  # Make a copy so the original stays unchanged
  dt <- copy(dataset)
  
  # Log transformation
  dt[, log_y := log(y)]
  
  # Forecast (point estimate) w-week(s) ahead 
  dt[, log_yhat := shift(log_y, n=w)]
  
  # Use the standard deviation of residuals to capture model uncertainty.
  dt[, resid := log_y - log_yhat]
  resid_sd <- sd(dt$resid, na.rm=T)
  
  # Generate draws for the w-week-ahead forecast, assuming a normal distribution
  draws <- exp(rnorm(d, mean=dt[nrow(dt),log_y], sd=resid_sd))
  
  return(draws)
}