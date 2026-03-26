nnetar_model <- function(dataset, w, d){
  
  # Description of function:
  # neural net model
  
  # Forecast (point estimate) w-week(s) ahead 
  data_ts <- ts(dataset$y)
  tmp_model <- nnetar(data_ts, P=1, p=2, size=10, repeats = 50, lambda = 0) 
  
  # Generate draws for the w-week-ahead forecast, assuming a normal distribution
  draws <- sapply(1:d,function(x)as.numeric(simulate(tmp_model, 
                                                     future = TRUE, nsim = w)))
  
  # Name the draws
  draws_dt <- as.data.table(draws)
  setnames(draws_dt, paste0("draw_", 1:d))
  
  # Organize the results
  ids <- data.table(model="nnetar_model", location_id = unique(dataset$location_id),
                    time_id = max(dataset$time_id)+w)
  draws_dt <- cbind(ids,draws_dt)
  
  return(draws_dt)
}
