# Ensemble
# Ensemble candidate models to generate predictions with uncertainty by location and time step

ensemble <- function(obs_dt, preds_dt, pipeline_inputs, param_set){
  
  source(file.path(pipeline_inputs$code_dir, "pipeline/model_performance_measures.R"))
  
  # Set some values
  configs <- pipeline_inputs$configs
  d <- configs$d
  list_of_models <- configs$models
  n_models <- length(list_of_models)
  
  # Ensembling the models is only applicable if there's more than one candidate model
  if (n_models > 1){
    
    # Find ensemble weights that optimize the performance measure of choice
    perform_func <- ifelse(configs$perform_meas=='MSE', calc_mse, calc_wis)
    
    # Define a vector of initial values that will result in equal weights
    vals <- logit(1/(n_models:2))
    
    # Identify the weights that optimize model performance
    if (length(vals)==1){
      fit <- optimize(f = perform_func, 
                      interval = c(-9,9),
                      forecasts = preds_dt[time_id<0],
                      observations = obs_dt,
                      list_of_models = list_of_models,
                      d = d)
      weights <- create_weights(fit$minimum)
    } else {
      fit <- optim(par = vals, 
                   fn = perform_func,
                   forecasts = preds_dt[time_id<0],
                   observations = obs_dt,
                   list_of_models = list_of_models,
                   d=d,
                   method = 'L-BFGS-B')
      weights <- create_weights(fit$par)
    }
    
    # Take a weighted average of model preds to get the ensemble preds.
    weights_dt <- data.table(model = list_of_models, weight = weights)   #globally fit p_s
    #weights_dt <- data.table(model = list_of_models, weight = c(1-param_set$p.s, param_set$p.s))   #known p_s
    preds_dt <- merge(preds_dt, weights_dt, by='model')
    
    # Calculate weighted average of each draw by location and time
    result <- preds_dt[, lapply(.SD, function(x) sum(x * weight)),
                       .SDcols = paste0("draw_",1:d), by=.(location_id, time_id)]
    result$model <- 'ensemble'
    setcolorder(result, "model")  
  } else {
    # use results from the only candidate model
    result <- preds_dt[, model := 'ensemble']
  }

  return(list(unadj_results = result, weights = weights_dt))
}