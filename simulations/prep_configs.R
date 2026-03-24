
prep_configs <- function(config_dir, out_dir){
  
  # Load the simulation config file
  config_sim <- read_yaml(file.path(config_dir,'config_sim.yaml'))
  write_yaml(config_sim, file.path(out_dir, 'config_sim.yaml'))
  
  # Enumerate parameter combinations and save table to disk
  beta1_vals <- if(!is.null(config_sim$linear_mod$beta1)) config_sim$linear_mod$beta1 else NA
  linear_mod_sigma_vals <- if(!is.null(config_sim$linear_mod$sigma)) config_sim$linear_mod$sigma else NA

  if(config_sim$use_ratio==F){
  params <- as.data.table(
    expand.grid(data_model = as.character(config_sim$data_model),
                fit_model = as.character(config_sim$fit_model),
                t = config_sim$t,
                theta = config_sim$theta,
                sigma.f = config_sim$sigma.f,
                sigma.s = config_sim$sigma.s,
                p.s = config_sim$p.s,
                y0 = config_sim$y0,
                beta1 = beta1_vals,
                linear_mod_sigma = linear_mod_sigma_vals,
                B = config_sim$B,
                R = config_sim$R,
                d = config_sim$d,
                L = config_sim$L,
                save_draws = config_sim$`save_draws?`,
                save_all_time_steps = config_sim$`save_all_time_steps?`,
                save_pre_adj_draws = config_sim$`save_pre_adj_draws?`,
                save_all_pre_adj_time_steps = config_sim$`save_all_pre_adj_time_steps?`,
                save_candidate_draws = config_sim$`save_candidate_draws?`
                )
  )
  } else {
    params <- as.data.table(
      expand.grid(data_model = as.character(config_sim$data_model),
                  fit_model = as.character(config_sim$fit_model),
                  t = config_sim$t,
                  theta = config_sim$theta,
                  signal_noise_ratio = as.numeric(config_sim$signal_noise_ratio),
                  p.s = config_sim$p.s,
                  y0 = config_sim$y0,
                  beta1 = beta1_vals,
                  linear_mod_sigma = linear_mod_sigma_vals,
                  B = config_sim$B,
                  R = config_sim$R,
                  d = config_sim$d,
                  L = config_sim$L,
                  save_draws = config_sim$`save_draws?`,
                  save_all_time_steps = config_sim$`save_all_time_steps?`,
                  save_pre_adj_draws = config_sim$`save_pre_adj_draws?`,
                  save_all_pre_adj_time_steps = config_sim$`save_all_pre_adj_time_steps?`,
                  save_candidate_draws = config_sim$`save_candidate_draws?`
      )
    )
  }
  params <- cbind(data.table(param_id=1:nrow(params)), params)
  
  if(config_sim$use_ratio==T){
    params[, sigma.f := theta / signal_noise_ratio]
    params[, sigma.s := theta / signal_noise_ratio]
  }
  
  # TEMP
  if(config_sim$use_ratio==F){
    params[, sigma.s := sigma.f]
  }
  
  fwrite(params, file.path(out_dir, 'params.csv'))
  
  return(params)
}
