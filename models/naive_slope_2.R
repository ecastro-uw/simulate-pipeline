
naive_slope_2 <- function(dataset, w, d){
  
  # Description of function:
  # Fit the model on the dataset provided and output d draws of the w-week-ahead forecast.
  # The naive slope 2 model takes the slope from the last 2 observed data points and propagates it
  # w weeks into the future. 
  
  # Make a copy so the original stays unchanged
  dt <- copy(dataset)
  
  # Log transformation
  dt[, log_y := log(y)]
  
  # Generate in-sample forecast  
  dt[, log_yhat := shift(log_y, n=w) + w * (shift(log_y, n=w) - shift(log_y, n=w+1))]
  
  # Use the standard deviation of residuals to capture model uncertainty.
  dt[, resid := log_y - log_yhat]
  resid_sd <- sd(dt$resid, na.rm=T)
  
  # Calculate point estimate for the OOS w-week-ahead forecast
  point_est <- last(dt$log_y) + diff(dt[, tail(.SD,2)]$log_y)*w
  
  # Generate draws for the w-week-ahead forecast, assuming a normal distribution
  draws <- exp(rnorm(d, mean=point_est, sd=resid_sd))
  
  return(draws)
}