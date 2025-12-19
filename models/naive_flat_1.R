
naive_flat_1 <- function(dataset, w, d){
  
  # Description of function:
  # Fit the model on the dataset provided and output d draws of the w-week-ahead forecast.
  # The naive flat 1 model takes the last observation and propagates it w weeks into the future.
  
  # Make a copy so the original stays unchanged
  dt <- copy(dataset)
  
  # Forecast (point estimate) w-week(s) ahead 
  dt[, yhat := shift(y, n=w), by=location_id]
  
  # Use the standard deviation of residuals to capture model uncertainty.
  dt[, resid := y - yhat]
  resid_sd <- sd(dt$resid, na.rm=T)
  #resid_sd <- param_set$sigma.f
  
  # For each location, generate draws for the w-week-ahead forecast, assuming a normal distribution
  last_t <- dt[time_id == max(dt$time_id)]
  draws_mat <- t(apply(last_t, 1, function(row){
    rnorm(d, mean=row["y"], sd=resid_sd)
  }))
  draws_dt <- as.data.table(draws_mat)
  setnames(draws_dt, paste0("draw_", 1:d))
  
  ids <- data.table(model='naive_flat_1', location_id=last_t$location_id, time_id=max(dt$time_id)+w, sigma = resid_sd)
  draws_dt <- cbind(ids,draws_dt)
  
  return(draws_dt)
}