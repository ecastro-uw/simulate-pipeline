# Model 5: Linear Auto-Regressive Model 
# Covariates: Deaths (total reported per 10K pop over last 2 weeks)
# Note: Currently only supports w=1 (hard coded)

model_5 <- function(dataset, w, d){
  
  # Make a copy so the original stays unchanged
  dt <- copy(dataset)
  setorder(dt, location_id, time_id)
  
  # Lag the dependent variable to use as a predictor
  dt[, lagged_y := shift(y), by=location_id]
  
  # Calculate sum of deaths over the previous 2 weeks (excluding current week)
  dt[, deaths_lag2_sum := frollsum(shift(deaths_pc, 1), n = 2, align = "right"), by = location_id]
  
  # Fit the model
  fit <- lm(y ~ lagged_y + deaths_lag2_sum, data = dt)
  
  # Get draws of the regression coefs 
  beta_draws <- mvrnorm(n = d, mu = coef(fit), Sigma = vcov(fit))
  
  # Generate data file for 1-week ahead predictions
  last_time_step <- max(dt$time_id)
  
  # Grab the last 2 time steps to reconstruct the rolling sum for the forecast row
  last_two <- dt[time_id %in% c(last_time_step - 1, last_time_step), 
                 .(location_id, time_id, y, deaths_pc)]
  
  # For the forecast row, deaths_lag2_sum = deaths_pc at t + deaths_pc at t-1
  new_dt <- last_two[, .(
    time_id        = last_time_step + 1,
    lagged_y       = y[time_id == last_time_step],
    deaths_lag2_sum = sum(deaths_pc)   # sums t and t-1
  ), by = location_id]
  
  # Construct a matrix with new data values
  X_new <- model.matrix(~lagged_y + deaths_lag2_sum, data=new_dt)
  
  # Compute draws of the fitted values
  fitted_draws <- beta_draws %*% t(X_new)
  
  # Add residual noise
  sigma_hat <- sigma(fit)
  noise <- matrix(rnorm(d * nrow(new_dt), mean = 0, sd = sigma_hat),
                  nrow = d, ncol = nrow(new_dt))
  
  # Get the final draws
  predictive_draws <- t(fitted_draws + noise)
  draws_dt <- as.data.table(predictive_draws)
  setnames(draws_dt, paste0("draw_", 1:d))
  
  # Check against predict()
  #predictions <- predict(fit, new_dt, interval="prediction")
  #apply(predictive_draws, 1, quantile, probs = c(0.025, 0.975))
  
  # Organize the results
  ids <- data.table(model="model_5", location_id = new_dt$location_id, time_id = new_dt$time_id, sigma = sigma_hat)
  draws_dt <- cbind(ids,draws_dt)
  
  return(draws_dt)
}