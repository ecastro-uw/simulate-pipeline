# simulate data

simulate_data <- function(param_set){
  
  # Define parameters
  data_model = as.character(param_set$data_model)
  t = param_set$t
  theta = param_set$theta
  sigma.f = param_set$sigma.f
  sigma.s = param_set$sigma.s
  p.s = param_set$p.s
  y0 = param_set$y0
  L = param_set$L
  
  # Simulate data - store in an L x (t+1) matrix 
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
  
  return(sim_dat)
}
