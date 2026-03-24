# simulate data
# Simulate an L x (t+w) matrix of data, where L is the number of locations (one row per loc),
# t is the number of time steps to be used for fitting and w is the number of time steps ahead
# you wish to forecast. 

simulate_data <- function(param_set, pipeline_inputs){

  # Define parameters
  data_model = as.character(param_set$data_model)
  t = param_set$t
  theta = param_set$theta
  sigma.f = param_set$sigma.f
  sigma.s = param_set$sigma.s
  p.s = param_set$p.s
  y0 = param_set$y0
  L = param_set$L
  w <- pipeline_inputs$configs$w

  # Simple linear model: y_{t+1} = beta1 * x_t + epsilon
  if(data_model == 'simple_lm'){
    beta1 <- param_set$beta1
    linear_mod_sigma <- param_set$linear_mod_sigma

    # Simulate x for L locations over t+1 time steps (iid standard normal)
    sim_x <- matrix(rnorm(L * (t + 1), mean = 0, sd = 1), nrow = L, ncol = t + 1)

    # Simulate y: y[,1] = y0; y[,j] = beta1 * x[,j-1] + epsilon for j = 2,...,t+1
    sim_y <- matrix(NA_real_, nrow = L, ncol = t + 1)
    sim_y[, 1] <- y0
    eps <- matrix(rnorm(L * t, mean = 0, sd = linear_mod_sigma), nrow = L, ncol = t)
    for (j in 2:(t + 1)) {
      sim_y[, j] <- beta1 * sim_x[, j - 1] + eps[, j - 1]
    }

    # Apply treatment effect at the final time step
    sim_y[, t + 1] <- sim_y[, t + 1] - theta

    # Reshape y to long format
    dt_y <- as.data.table(sim_y)
    dt_y[, location_id := .I]
    dt_y <- melt(dt_y, id.vars = 'location_id', variable.name = 'time_id', value.name = 'y')
    dt_y[, time_id := as.numeric(sub('V', '', time_id)) - (t + 1)]

    # Reshape x to long format
    dt_x <- as.data.table(sim_x)
    dt_x[, location_id := .I]
    dt_x <- melt(dt_x, id.vars = 'location_id', variable.name = 'time_id', value.name = 'x')
    dt_x[, time_id := as.numeric(sub('V', '', time_id)) - (t + 1)]

    # Merge and return
    dt <- merge(dt_y, dt_x, by = c('location_id', 'time_id'))
    dt <- dt[order(location_id, time_id)]
    return(dt)
  }
  
  # Simulate data - L x (t+1) matrix 
  if(data_model %in% c("naive_flat_1", "ensemble")){
    changes <- matrix(rnorm(L*t, mean=0, sd=sigma.f), ncol=t)
    tf <- t(apply(changes, 1, cumsum))
    tf <- tf + y0
    tf <- cbind(matrix(y0, nrow=L, ncol=1), tf)
    tf[,t+1] <- tf[,t+1] - theta
  }
  
  if(data_model %in% c("naive_slope_2", "ensemble")){
    
    weight_matrix <- function(n) {
      outer(1:n, 1:n, function(i, j) ifelse(i >= j, i - j + 1, 0))
    }
    
    transform_vector <- function(d) {
      W <- weight_matrix(length(d))
      as.vector(W %*% d)
    }
    
    if(sigma.s==0){
      a <- matrix(rnorm(L, mean=0, sd=0.3), ncol=1)
      b <- matrix(rnorm(L*(t-1), mean=0, sd=sigma.s), ncol=t-1)
      c <- cbind(a,b)
    } else {
      c <- matrix(rnorm(L*t, mean=0, sd=sigma.s), ncol=t)
    }
    
    ts <- t(apply(c, 1, transform_vector))
    ts <- ts + y0
    ts <- cbind(matrix(y0, nrow=L, ncol=1), ts)
    ts[,t+1] <- ts[,t+1] - theta
  }
  
  if(data_model == "ensemble"){
    sim_dat <- tf*(1-p.s) + ts*p.s
  }
  
  if(data_model=='naive_flat_1'){
    sim_dat <- tf
  }
  if(data_model=='naive_slope_2'){
    sim_dat <- ts
  }
  
  # reshape
  dt <- as.data.table(sim_dat)
  dt[, location_id := .I]
  dt <- melt(dt, id.vars = 'location_id',
             variable.name = "time_id",
             value.name = "y")
  dt[, time_id := as.numeric(sub('V','', time_id)) - (t+1)]
  dt <- dt[order(location_id, time_id)]
  
  
  return(dt)
}
