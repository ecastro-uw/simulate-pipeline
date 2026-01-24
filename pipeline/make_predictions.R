# Make predictions
# Generates predictions with uncertainty by model, location, and time step

make_predictions <- function(data, pipeline_inputs){
  
  # Set some values
  configs <- pipeline_inputs$configs
  list_of_models <- configs$models
  w <- configs$w
  d <- configs$d
  min_train_t <- pipeline_inputs$min_train_t
  max_train_t <- pipeline_inputs$max_train_t
  spread <- max_train_t - min_train_t + w # total number of weeks to be forecast (pre + post event)
  
  results <- vector("list", )
  # Use each model to forecast the desired number of time steps
  pred_all <- function(mod, dt, spread, min_train_t, max_train_t, w, d){
    model <- get(mod)
    # For each model, provide n weeks of training data and the model will produce d forecasts for the
    # (n+w)th week. For all models, let n vary from min_train_t to max_train_t.
    list_of_results <- vector("list", spread)
    index <- 0
    for(ts in min_train_t:max_train_t){
      index <- index+1
      time_steps <- unique(dt$time_id)[1:ts]
      train_dt <- dt[time_id %in% time_steps]
      list_of_results[[index]] <- model(dataset=train_dt,w,d)
    }
    results_one_model <- rbindlist(list_of_results)

    return(results_one_model)
  }
  predictions_dt <- rbindlist(lapply(list_of_models, pred_all, data, spread, min_train_t, max_train_t, w, d))
  
  return(predictions_dt)
}
