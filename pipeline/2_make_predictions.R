# Make predictions
# Generates predictions with uncertainty by model, location, and time step

make_predictions <- function(data, pipeline_inputs){
  
  # Set some values
  configs <- pipeline_inputs$configs
  list_of_models <- configs$models
  w <- configs$w
  d <- configs$d
  spread <- 4 # total number of weeks to be forecast (3 weeks pre + 1 week post event)
  
  results <- vector("list", )
  # Use each model to forecast the desired number of time steps
  pred_all <- function(mod, dt, spread, w, d){
    model <- get(mod)
    
    # For each model, provide n weeks of training data and the model will produce d forecasts for the
    # (n+w)th week. Iteratively hold out the last 4, 3, 2, and 1 weeks of data to make out-of-sample preds.
    list_of_results <- vector("list", spread)
    hold_out <- seq(spread, w)
    for (i in seq_along(hold_out)){
      drop_n <- hold_out[i]
      train_dt <- dt[time_id <= (-drop_n)]
      list_of_results[[i]] <- model(dataset=train_dt, w, d)
    }
    results_one_model <- rbindlist(list_of_results)

    return(results_one_model)
  }
  predictions_dt <- rbindlist(lapply(list_of_models, pred_all, data, spread, w, d))
  
  return(predictions_dt)
}
