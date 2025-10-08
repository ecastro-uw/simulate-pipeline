# pipeline

pipeline <- function(param_set, pipeline_inputs){
  
  root_dir <- pipeline_inputs$root_dir
  
  source(file.path(root_dir,'code/simulations/refactor/simulate_data.R'))
  source(file.path(root_dir,'code/simulations/refactor/fit_ensemble.R'))
  source(file.path(root_dir,'code/pipeline/adjust_UI.R'))
  
  # Simulate data
  sim_dat <- simulate_data(param_set)
  
  # Fit the model
  results_draws <- fit_ensemble(sim_dat, pipeline_inputs)
  
  # Adjust the UI
  final_results <- adjust_UI(results_draws, pipeline_inputs$problem_log, pipeline_inputs$configs)
  
  # Summarize results
  summarize <- function(results_one_loc){
    
    # observations
    obs_tmp <- results_one_loc$obs
    
    # forecasts
    pred_tmp <- results_one_loc$ensemble_adj #e.g. for one location
    cols <- names(pred_tmp)[-1]
  
    dt <- data.table(
      time_id = pred_tmp$time_id,
      q1    = pred_tmp[, apply(.SD, 1, quantile, 0.010), .SDcols = cols],
      q2.5  = pred_tmp[, apply(.SD, 1, quantile, 0.025), .SDcols = cols],
      q50   = pred_tmp[, apply(.SD, 1, quantile, 0.500), .SDcols = cols],
      q97.5 = pred_tmp[, apply(.SD, 1, quantile, 0.975), .SDcols = cols]
    )
    
    #p-val
    p_tmp <- results_one_loc$p_values
    
    # combine
    dt <- merge(dt, obs_tmp, by='time_id', all = T)
    dt <- merge(dt, p_tmp, by='time_id', all=T)
  
  }
  
  results_summary <- rbindlist(lapply(final_results, summarize))
  
  return(results_summary)
}