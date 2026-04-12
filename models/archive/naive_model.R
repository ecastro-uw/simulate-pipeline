naive_model <- function(dataset, w, d){
  
  # Description of function:
  # 
  
  # Forecast (point estimate) w-week(s) ahead 
  data_ts <- ts(dataset$visit_tot)
  
  tmp_mod <- naive(data_ts, h = w, lambda = 0,
                   level = seq(51,99,by=1))
  tmp_out <- cbind(data.frame(tmp_mod$lower), data.frame(tmp_mod$upper))
  tmp_sample <- sample(1:length(tmp_out), d, replace = TRUE)
  draws <- unname(tmp_out[tmp_sample])
  
  
  return(draws)
}