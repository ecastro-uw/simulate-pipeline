# fit ensemble

fit_ensemble <- function(sim_dat, pipeline_inputs){
  
  # Source the ensemble functions
  root_dir <- pipeline_inputs$root_dir
  source(file.path(root_dir, "code/pipeline/ensemble_and_forecast.R"))
  source(file.path(root_dir, "code/pipeline/model_performance_measures.R"))
  
  # Define parameters
  w <- pipeline_inputs$configs$w
  t <- ncol(sim_dat) - w
  
  results <- list()
  # Call the function once per location
  for(l in 1:nrow(sim_dat)){
    
    # Convert each row to a data table
    dt <- data.table(time_id=-t:(w-1), y=exp(sim_dat[l,]))
    
    # Use the simulated data to fit and forecast
    result <- ensemble_and_forecast(
      county_dt = dt,
      configs = pipeline_inputs[[1]],
      min_train_t = pipeline_inputs[[2]],
      max_train_t = pipeline_inputs[[3]]
    )
    
    # save draws
    results[[as.character(l)]] <- result
    #results <- c(results, list(result))
  }
  
  return(results)

}