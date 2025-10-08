# Ensemble and Forecast
library(boot)

# Description: This function runs an interrupted time series (ITS) analysis for a single county-level instance of 
# a pre-specified mandate type (e.g. restaurant closure). The user specifies a list of candidate models to be  
# ensembled, the criteria used to evaluate model performance, and the number of weeks ahead they wish to forecast.
# Each model generates d-draws of w-week-ahead predictions. Number of draws is also user-specified.

ensemble_and_forecast <- function(county_dt, configs, min_train_t, max_train_t) {
  
  # Set the seed
  #set.seed(configs$seed)
  
  # Set some values
  list_of_models <- configs$models
  n_models <- length(list_of_models)
  w <- configs$w
  d <- configs$d
  spread <- max_train_t - min_train_t + w # total number of weeks to be forecast (pre + post event)
  
  # Do not allow y values of 0 (for log-normal models)
  # Set the visit volume to be 1/10th of the smallest observed non-zero visit
  if(nrow(county_dt[time_id<0 & y==0])>0){
    epsilon <- min(county_dt[time_id<0 & y!=0]$y)/10
    county_dt[time_id<0 & y==0, y:=epsilon]
  }
  
  # Create a vector of observations (needed later for ensemble procedure)
  #observations <- county_dt[(min_train_t+w):(nrow(county_dt)-w),y]
  observations <- county_dt[-c(1:min_train_t,max_train_t+1:w), y]
  
  # Results will be stored as a list of data tables, one for each model type plus an extra table for observed values.
  # Each data table is long by time_id and wide by draw.
  
  # First, create an empty data table as a template
  col_names <- c('time_id', paste0('draw_', 1:d))
  my_data <- data.table(matrix(nrow=spread, ncol = length(col_names)))
  colnames(my_data) <- col_names
  my_data$time_id <- (min_train_t-max_train_t):(w-1)
  if(nrow(my_data)>1){
    my_data <- as.data.table(sapply(my_data, as.numeric))
  } else {
    my_data <- my_data[, lapply(.SD, as.numeric)]
  }
  
  # Now, use each model to forecast for the desired number of time steps
  pred_all <- function(mod, dt, spread, min_train_t, w, d){
    model <- get(mod)
    # For each model, provide n weeks of data and the model will produce d forecasts for the
    # (n+w)th week. For all models, let n vary from min_train_t to max_train_t.
    for (t in 1:spread){
      train_dt <- dt[1:(min_train_t+t-1)]
      my_data[t,2:(d+1)] <- as.list(model(dataset=train_dt, w, d))
    }
    return(my_data)
  }
  forecasts <- lapply(list_of_models, pred_all, county_dt, spread, min_train_t, w, d)

  # Assign names
  names(forecasts) <- list_of_models
  
  # Create a version with only pre-event forecasts (needed for ensemble procedure)
  forecasts_pre <- lapply(1:n_models, function(x) forecasts[[x]][time_id %in% (min_train_t - max_train_t):(-1)])
  names(forecasts_pre) <- list_of_models
  
  # Ensemble the models (only necessary if more than one candidate model)
  if (n_models > 1){
    
    # Find ensemble weights that optimize the performance measure of choice
    perform_func <- ifelse(configs$perform_meas=='MSE', calc_mse, calc_wis)
    
    # Define a vector of initial values that will result in equal weights
    vals <- logit(1/(n_models:2))
    
    # Identify the weights that optimize model performance
    if (length(vals)==1){
      fit <- optimize(f = perform_func, 
                      interval = c(-9,9),
                      forecasts = forecasts_pre,
                      observations = observations)
      weights <- create_weights(fit$minimum)
    } else {
      fit <- optim(par = vals, 
                   fn = perform_func,
                   forecasts = forecasts_pre,
                   observations = observations,
                   method = 'L-BFGS-B')
      weights <- create_weights(fit$par)
    }
    
    # Take a weighted average of model preds to get the ensemble preds. Save to results object.
    time_id <- (min_train_t - max_train_t):(w-1)
    forecasts$ensemble <- cbind(time_id, Reduce(`+`, Map(`*`, lapply(forecasts, function(x) x[,-1]), weights))) 
    
    # Save the weights to the results object
    forecasts$ensemble_weights <- data.table(model=list_of_models, ens_weight=weights)
  } 
  
  # Add observed values to the list of results for completeness
  forecasts <- append(forecasts, list(county_dt), after=0)
  names(forecasts)[1] <- 'obs'
  
  # Summarize the results and add to the results table
  #model_list <- names(forecasts)[-1]
  #summarize_results <- function(model_name){
  #  draws <- forecasts[[model_name]][,-1]
  #  temp <- data.table(wk_id = (min_train_t+w):(max_train_t+w),
  #                     type = model_name,
  #                     val = rowMeans(draws),
  #                     lower = apply(draws,1,quantile,0.025),
  #                     upper = apply(draws,1,quantile,0.975))
  #  return(temp)
  #}
  #results_dt <- rbind(results_dt, rbindlist(lapply(model_list,summarize_results)))
  #results_dt[,  `:=` (location_id=row$location_id, num=row$num, mand_on=ifelse(wk_id<(max_train_t + 1), 0, 1))]
  #results_dt <- results_dt[, .(num, location_id, wk_id, mand_on, type, val, lower, upper)]
  
  #return(list(results_dt, forecasts))
  return(forecasts)
}
