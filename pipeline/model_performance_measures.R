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
calc_mse <- function(vals, forecasts, observations){
  
  # convert n real number vals into n+1 weights that sum to 1
  weights <- create_weights(vals)
  
  # For each model, calc the mean prediction for each week
  mean_preds <- lapply(seq(1, length(weights)), function(x) rowMeans(forecasts[[x]][,-1]))

  # Take a weighted average of model preds to get the ensemble preds
  predictions <- Reduce(`+`, Map(`*`, mean_preds, weights))
  
  # Calculate MSE
  mse <- mean((observations - predictions)^2)
  
  return(mse)
}


# 2. WIS
calc_wis <- function(weights, mat_list){
  # define this function 
}
