# pipeline

pipeline <- function(param_set, pipeline_inputs){
  
  code_dir <- pipeline_inputs$code_dir
  
  source(file.path(code_dir,'simulations/simulate_data.R'))
  source(file.path(code_dir,'pipeline/make_predictions.R'))
  source(file.path(code_dir,'pipeline/ensemble.R'))
  source(file.path(code_dir,'pipeline/adjust_UI.R'))
  
  ## PIPELINE 
  # (1) Simulate data
  sim_start <- Sys.time()
  sim_dat <- simulate_data(param_set, pipeline_inputs)
  sim_end <- Sys.time()
  sim_time <- sim_end - sim_start
  
  # (2) Make predictions
  pred_start <- Sys.time()
  preds_by_model <- make_predictions(sim_dat, pipeline_inputs)
  sigmas_dt <- unique(preds_by_model[,.(model, time_id, sigma)])
  pred_end <- Sys.time()
  pred_time <- pred_end - pred_start
  
  # (3) Ensemble
  ensemble_start <- Sys.time()
  ensemble_out_list <- ensemble(sim_dat, preds_by_model, pipeline_inputs, param_set)
  unadj_results <- ensemble_out_list$unadj_results
  weights_dt <- ensemble_out_list$weights
  ensemble_end <- Sys.time()
  ensemble_time <- ensemble_end - ensemble_start
  
  # (4) Adjust the UI
  adjust_start <- Sys.time()
  adjusted_results <- adjust_UI(sim_dat, unadj_results, pipeline_inputs$problem_log, pipeline_inputs$configs)
  final_results <- adjusted_results$final_results
  adjust_end <- Sys.time()
  adjust_time <- adjust_end - adjust_start
  
  
  ## PREP RESULTS FOR OUTPUT
  draw_cols <- paste0('draw_', 1:pipeline_inputs$configs$d)
  
  # (1) Observations
  # sim_dat
  
  # (2) Candidate model estimates
  if(param_set$save_candidate_draws==T){
    candidate_mod_output <- preds_by_model[, .SD, .SDcols = c('model','location_id', 'time_id', draw_cols)]
  } else {
    candidate_mod_output <- preds_by_model[, `:=` (q2.5 = apply(.SD, 1, quantile, 0.025),
                                                   q50 = apply(.SD, 1, quantile, 0.50),
                                                   q97.5 = apply(.SD, 1, quantile, 0.975),
                                                   mean = apply(.SD, 1, mean)),
                                           .SDcols = draw_cols][,.(model, location_id, time_id, q2.5, q50, q97.5, mean)]
  }
  
  # (3) Pre-adjustment ensemble estimates
  forecast_target <- pipeline_inputs$configs$w - 1
  if(param_set$save_pre_adj_draws==T){
    if(param_set$save_all_pre_adj_time_steps==T){
      # save out draws all time steps
      pre_adj_output <- unadj_results[, .SD, .SDcols = c('location_id', 'time_id', draw_cols)]
    } else {
      # save out draws, only time step of interest
      pre_adj_output <- unadj_results[time_id==forecast_target, .SD, .SDcols = c('location_id', 'time_id', paste0('draw_',1:pipeline_inputs$configs$d))]
    }
  } else {
    if(param_set$save_all_pre_adj_time_steps==T){
      # save out summary stats, all time steps
      pre_adj_output <- unadj_results[, `:=` (q1 = apply(.SD, 1, quantile, 0.01),
                                              q2.5 = apply(.SD, 1, quantile, 0.025),
                                              q50 = apply(.SD, 1, quantile, 0.50),
                                              q97.5 = apply(.SD, 1, quantile, 0.975)),
                                      .SDcols = draw_cols][,.(location_id, time_id, q1, q2.5, q50, mean, q97.5)]
    } else {
      # save out summary stats, only time step of interest
      pre_adj_output <- unadj_results[time_id==forecast_target][,
                                      `:=` (q1 = apply(.SD, 1, quantile, 0.01),
                                            q2.5 = apply(.SD, 1, quantile, 0.025),
                                            q50 = apply(.SD, 1, quantile, 0.50),
                                            q97.5 = apply(.SD, 1, quantile, 0.975)),
                                      .SDcols = draw_cols][,.(location_id, time_id, q1, q2.5, q50, mean, q97.5)]
    }
  }
  
  # (4) Pre-adjustment coverage rate
  coverage_pre <- adjusted_results$coverage_pre
  
  # (5) Post-adjustment coverage rate
  coverage_post <- adjusted_results$coverage_post
  
  # (6) Multiplier
  multiplier <- adjusted_results$multiplier

  # (7) Adjusted forecasts
  if(param_set$save_draws==T){
    if(param_set$save_all_time_steps==T){ 
      # save out draws, all time steps
      results_output <- final_results[, .SD, .SDcols = c('location_id', 'time_id','p_val', draw_cols)]
    } else{
      # save out draws, only time step of interest
      results_output <- final_results[time_id==forecast_target, .SD, .SDcols = c('location_id', 'time_id','p_val', draw_cols)]
    }
  } else { 
    if(param_set$save_all_time_steps==T){ 
      # save out summary stats, all time steps
      results_output <- final_results[, `:=` (q1 = apply(.SD, 1, quantile, 0.01),
                                              q2.5 = apply(.SD, 1, quantile, 0.025),
                                              q50 = apply(.SD, 1, quantile, 0.50),
                                              q97.5 = apply(.SD, 1, quantile, 0.975)),
                                      .SDcols = draw_cols][,.(location_id, time_id, q1, q2.5, q50, mean=y, q97.5, p_val)]
    } else{
      # save out summary, only time step of interest
      results_output <- final_results[time_id==forecast_target][,
                                      `:=` (q1 = apply(.SD, 1, quantile, 0.01),
                                            q2.5 = apply(.SD, 1, quantile, 0.025),
                                            q50 = apply(.SD, 1, quantile, 0.50),
                                            q97.5 = apply(.SD, 1, quantile, 0.975)),
                                      .SDcols = draw_cols][,.(location_id, time_id, q1, q2.5, q50, mean=y, q97.5, p_val)]
    }
  }
  
  # (8) Ensemble weights
  # weights_dt
  
  # (9) Sigmas
  # sigmas_dt
  
  # (10) Time stamps
  time_stamps <- c(sim_time = sim_time, pred_time = pred_time, ensemble_time = ensemble_time, adjust_time = adjust_time)
  
  return(list(obs_dt = sim_dat,
              candidate_mod_output = candidate_mod_output,
              pre_adj_output = pre_adj_output,
              coverage_pre = coverage_pre,
              coverage_post = coverage_post,
              multiplier = multiplier,
              results_output = results_output,
              weights_dt = weights_dt,
              sigmas_dt = sigmas_dt,
              time_stamps = time_stamps))
}