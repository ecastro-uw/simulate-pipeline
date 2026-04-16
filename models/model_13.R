# Model 13: Linear Auto-Regressive Model 
# Covariates: Gathering restrictions, School closures, Bar closures, Gym closures
# Note: Currently only supports w=1 (hard coded)

model_13 <- function(dataset, w, d){
  
  # Make a copy so the original stays unchanged
  dt <- copy(dataset)
  
  # Lag the dependent variable to use as a predictor
  dt[, lagged_y := shift(y), by=location_id]
  
  # Lag the mandate variables to use as predictors
  dt[, `:=` (lagged_gathering = shift(pct_gathering),
             lagged_edu = shift(pct_edu),
             lagged_bar = shift(pct_bar),
             lagged_gym = shift(pct_gym)),
     by=location_id]
  
  # Fit the model
  fit <- lm(y ~ lagged_y + lagged_gathering + lagged_edu + lagged_bar + lagged_gym, data = dt)
  
  # Get draws of the regression coefs 
  beta_draws <- mvrnorm(n = d, mu = coef(fit), Sigma = vcov(fit))
  
  # Generate data file for 1-week ahead predictions
  last_time_step <- max(dt$time_id)
  new_dt <- dt[time_id==last_time_step, .(location_id, time_id, y, pct_gathering, pct_edu, pct_bar, pct_gym)]
  new_dt$time_id <- last_time_step + 1
  setnames(new_dt, c('y', 'pct_gathering', 'pct_edu', 'pct_bar', 'pct_gym'),
           c('lagged_y', 'lagged_gathering', 'lagged_edu', 'lagged_bar', 'lagged_gym'))
  
  # Construct a matrix with new data values
  X_new <- model.matrix(~lagged_y + lagged_gathering + lagged_edu + lagged_bar + lagged_gym, data=new_dt)
  
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
  
  # Organize the results
  ids <- data.table(model="model_13", location_id = new_dt$location_id, time_id = new_dt$time_id, sigma = sigma_hat)
  draws_dt <- cbind(ids,draws_dt)
  
  return(draws_dt)
}