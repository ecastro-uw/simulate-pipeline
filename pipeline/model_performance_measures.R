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
calc_wis <- function(vals, forecasts, observations, list_of_models, d){
  
  # Convert n real number vals into n+1 weights that sum to 1
  weights <- create_weights(vals)
  weights_dt <- data.table(model = list_of_models, weight = weights)
  
  # Ensemble
  draw_cols <- paste0("draw_", 1:d)
  forecasts_w <- merge(forecasts, weights_dt, by="model") # add weights
  forecasts_w[, (draw_cols) := lapply(.SD, function(x) x * weight), .SDcols = draw_cols] #multiply each draw by its weight
  ensemble <- forecasts_w[, lapply(.SD, sum), by = .(location_id, time_id), .SDcols = draw_cols] # sum weighted draws across models (by loc and time)
  
  # Calculate quantiles of draws by location and time
  pi_probs <- c(0.01, 0.025, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45,
                0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975, 0.99)
  pi_names <- paste0('q', pi_probs * 100)
  quantile_dt <- as.data.table(t(apply(ensemble[, .SD, .SDcols = draw_cols], 1, quantile, pi_probs)))
  setnames(quantile_dt, pi_names)
  ensemble_summary <- cbind(ensemble[, .(location_id, time_id)], quantile_dt)
  
  # Calculate WIS
  time_steps <- unique(ensemble$time_id)
  obs <- observations[time_id %in% time_steps, .(location_id, time_id, y)]
  obs <- obs[order(location_id, time_id)] # ensure identical sort order for obs and preds
  ensemble_summary <- ensemble_summary[order(location_id, time_id)]
  
  scores <- wis(observed = obs$y, 
                predicted = as.matrix(ensemble_summary[, .SD, .SDcols = pi_names]),
                quantile = pi_probs)
  wis_score <- sum(scores)

  return(wis_score)
}
