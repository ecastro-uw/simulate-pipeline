nnetar_model <- function(dataset, w, d){
  
  # Description of function:
  # neural net model
  
  # Forecast (point estimate) w-week(s) ahead 
  data_ts <- ts(dataset$visit_tot)
  tmp_model <- nnetar(data_ts, repeats = 50, size = 10, p=2, P = 1, lambda = 0) 
  
  # Generate draws for the w-week-ahead forecast, assuming a normal distribution
  draws <- sapply(1:d,function(x)as.numeric(simulate(tmp_model, 
                                                     future = TRUE, nsim = w)))
  
  return(draws)
}
