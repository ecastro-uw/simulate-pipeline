# Measures of Model Performance

# 0. Create weights that always sum to 1
create_weights <- function(vals){
  weights <- rep(0, length(vals)+1) 
  remaining <- 1 
  for (mod_num in 1:length(vals)){  
    weights[mod_num] <- remaining * inv.logit(vals[mod_num])
    remaining <- 1 - sum(weights)
  }
  weights[length(vals)+1] <- remaining
  weights[is.na(weights)] <- 0
  return(weights)
}


# 1. MSE
calc_mse <- function(vals, forecasts, observations, list_of_models, d){
  
  # Convert n real number vals into n+1 weights that sum to 1
  weights <- create_weights(vals)
  weights_dt <- data.table(model = list_of_models, weight = weights)
  
  # For each model, location, and time step, calculate the mean prediction
  forecasts[, mean := rowMeans(.SD), .SDcols = paste0("draw_",1:d)]
  
  # Merge weights
  small_dt <- merge(forecasts[,.(model, location_id, time_id, mean)], weights_dt, by='model')
  
  # Calculate weighted average by location and time
  ensemble_dt <- small_dt[, .(weighted_mean = sum(mean * weight)), by=.(location_id, time_id)]
  
  # Merge observations
  ensemble_dt <- merge(ensemble_dt, observations, by=c('location_id', 'time_id'), all.x=T)
  
  # Calculate MSE
  mse <- mean((ensemble_dt$y - ensemble_dt$weighted_mean)^2)
  
  return(mse)
}


# 2. WIS
calc_wis <- function(weights, mat_list){
  # define this function 
}
