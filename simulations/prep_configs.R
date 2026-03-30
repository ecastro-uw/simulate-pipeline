
prep_configs <- function(config_dir, out_dir){
  
  # Load the simulation config file
  config_sim <- read_yaml(file.path(config_dir,'config_sim.yaml'))
  write_yaml(config_sim, file.path(out_dir, 'config_sim.yaml'))
  
  # Enumerate parameter combinations and save table to disk
  if(config_sim$data_model=='simple_lm'){
    params <- as.data.table(
      expand.grid(data_model = as.character(config_sim$data_model),
                  fit_model = as.character(config_sim$fit_model),
                  t = config_sim$t,
                  theta = config_sim$theta,
                  y0 = config_sim$y0,
                  beta0 = config_sim$simple_lm$beta0,
                  beta1 = config_sim$simple_lm$beta1,
                  sigma = config_sim$simple_lm$sigma,
                  B = config_sim$B,
                  R = config_sim$R,
                  d = config_sim$d,
                  L = config_sim$L))
  }
  
  if(config_sim$data_model=='ar_simple_lm'){
    params <- as.data.table(
      expand.grid(data_model = as.character(config_sim$data_model),
                  fit_model = as.character(config_sim$fit_model),
                  t = config_sim$t,
                  theta = config_sim$theta,
                  y0 = config_sim$y0,
                  beta0 = config_sim$ar_simple_lm$beta0,
                  beta1 = config_sim$ar_simple_lm$beta1,
                  beta2 = config_sim$ar_simple_lm$beta2,
                  sigma = config_sim$ar_simple_lm$sigma,
                  B = config_sim$B,
                  R = config_sim$R,
                  d = config_sim$d,
                  L = config_sim$L))
  }
  
  if(config_sim$data_model == 'arima_model'){
    params <- as.data.table(
      expand.grid(data_model = as.character(config_sim$data_model),
                  fit_model = as.character(config_sim$fit_model),
                  t = config_sim$t,
                  theta = config_sim$theta,
                  y0 = config_sim$y0,
                  phi = config_sim$arima_model$phi,
                  mu = config_sim$arima_model$mu,
                  sigma = config_sim$arima_model$sigma,
                  B = config_sim$B,
                  R = config_sim$R,
                  d = config_sim$d,
                  L = config_sim$L))
  }

  if (config_sim$data_model %in% c('naive_flat_1', 'naive_slope_2', 'ensemble')){
    if(config_sim$naive_mods$use_ratio==F){
      params <- as.data.table(
        expand.grid(data_model = as.character(config_sim$data_model),
                    fit_model = as.character(config_sim$fit_model),
                    t = config_sim$t,
                    theta = config_sim$theta,
                    sigma.f = config_sim$naive_mods$sigma.f,
                    sigma.s = config_sim$naive_mods$sigma.s,
                    p.s = config_sim$p.s,
                    y0 = config_sim$y0,
                    B = config_sim$B,
                    R = config_sim$R,
                    d = config_sim$d,
                    L = config_sim$L))
      
      # Constrain sigmas to be equal
      params[, sigma.s := sigma.f]
      
    } else {
      params <- as.data.table(
        expand.grid(data_model = as.character(config_sim$data_model),
                    fit_model = as.character(config_sim$fit_model),
                    t = config_sim$t,
                    theta = config_sim$theta,
                    signal_noise_ratio = as.numeric(config_sim$naive_mods$signal_noise_ratio),
                    p.s = config_sim$p.s,
                    y0 = config_sim$y0,
                    B = config_sim$B,
                    R = config_sim$R,
                    d = config_sim$d,
                    L = config_sim$L))
      
      # Define sigmas based upon signal-to-noise ratio
      params[, sigma.f := theta / signal_noise_ratio]
      params[, sigma.s := theta / signal_noise_ratio]
    }
  }
  
  params <- cbind(data.table(param_id=1:nrow(params)), params)
  fwrite(params, file.path(out_dir, 'params.csv'))
  
  return(params)
}
