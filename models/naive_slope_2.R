
naive_slope_2 <- function(dataset, w, d){
  
  # Description of function:
  # Fit the model on the dataset provided and output d draws of the w-week-ahead forecast.
  # The naive slope 2 model takes the slope from the last 2 observed data points and propagates it
  # w weeks into the future. 
  
  # Make a copy so the original stays unchanged
  dt <- copy(dataset)
  
  # Generate in-sample forecast  
  dt[, yhat := shift(y, n=w) + w * (shift(y, n=w) - shift(y, n=w+1)), by=location_id]
  
  # Use the standard deviation of residuals to capture model uncertainty.
  dt[, resid := y - yhat]
  resid_sd <- sd(dt$resid, na.rm=T) #globally fit sigma
  #local_resids_sd <- dt[, sd(resid, na.rm=T), by=location_id] #locally fit sigmas
  #resid_sd <- param_set$sigma.s  #known sigma
  
  # For each location, calculate point estimate for the OOS w-week-ahead forecast
  point_est_dt <- dt[, .(point_est = last(y) + diff(tail(.SD, 2)$y) * w), 
                     by = location_id]
  
  # For each location, generate draws for the w-week-ahead forecast, assuming a normal distribution
  draws_mat <- t(apply(point_est_dt, 1, function(row){
    rnorm(d, mean=row["point_est"], sd=resid_sd)
  }))
  draws_dt <- as.data.table(draws_mat)
  setnames(draws_dt, paste0("draw_", 1:d))
  
  ids <- data.table(model='naive_slope_2', location_id=point_est_dt$location_id, time_id=max(dt$time_id)+w, sigma = resid_sd)
  draws_dt <- cbind(ids,draws_dt)
  
  return(draws_dt)
}