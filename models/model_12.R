# Model 12: auto.arima 
# Covariates: None

model_12 <- function(dataset, w, d) {
 
  dt <- copy(dataset)
  
  locations <- unique(dt$location_id)
  results_list <- vector("list", length(locations))
  
  for (i in seq_along(locations)) {
    loc      <- locations[i]
    loc_data <- dt[location_id == loc]
    setorder(loc_data, time_id)
    
    data_ts <- ts(loc_data$y)
    
    tmp_model <- auto.arima(data_ts)
    
    # Generate d simulation draws for the w-week-ahead forecast.
    # Each simulate() call draws one stochastic path; we keep only step w.
    draws <- sapply(seq_len(d), function(x) {
      sim <- as.numeric(simulate(tmp_model, future = TRUE, nsim = w))
      sim[w]
    })
    
    # Compute sigma as RMSE of in-sample residuals
    resids <- residuals(tmp_model)
    sigma  <- sqrt(mean(resids^2, na.rm = TRUE))
    
    draws_dt <- as.data.table(t(draws))
    setnames(draws_dt, paste0("draw_", seq_len(d)))
    
    ids <- data.table(
      model       = "model_12",
      location_id = loc,
      time_id     = max(loc_data$time_id) + w,
      sigma       = sigma
      #p           = arimaorder(tmp_model)[1],
      #d           = arimaorder(tmp_model)[2],
      #q           = arimaorder(tmp_model)[3]
    )
    results_list[[i]] <- cbind(ids, draws_dt)
  }
  
  return(rbindlist(results_list))
  
   
}  