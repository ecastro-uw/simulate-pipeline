ar_simple_lm <- function(dataset, w, d){
  
  # Make a copy so the original stays unchanged
  dt <- copy(dataset)
  
  # Lag the dependent variable to use as a predictor
  dt[, lagged_y := shift(y), by=location_id]
  
  # Lag the covariate to use as a predictor
  dt[, lagged_x := shift(x), by=location_id]
  
  # Fit the model
  fit <- lm(y ~ lagged_y + lagged_x, data = dt)
  
  # Get draws of the regression coefs 
  beta_draws <- mvrnorm(n = d, mu = coef(fit), Sigma = vcov(fit))
  
  # Generate data file for 1-week ahead predictions
  last_time_step <- max(dt$time_id)
  new_dt <- dt[time_id==last_time_step, .(location_id, time_id, y, x)]
  new_dt$time_id <- last_time_step + 1
  setnames(new_dt, c('y', 'x'), c('lagged_y', 'lagged_x'))
  
  # Construct a matrix with new data values
  X_new <- model.matrix(~lagged_y + lagged_x, data=new_dt)
  
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
  ids <- data.table(model="ar_simple_lm", location_id = new_dt$location_id, time_id = new_dt$time_id, sigma = sigma_hat)
  draws_dt <- cbind(ids,draws_dt)
  
  return(draws_dt)
  # draws_dt has one row per location and the time_id is equal to one step beyond
  # the last time_id of the training set. the file is wide by draw.
  # model, location_id, time_id, draw_1, ..., draw_d
}