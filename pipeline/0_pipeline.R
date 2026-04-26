# pipeline

pipeline <- function(pipeline_inputs, param_set=NULL){
  
  code_dir <- pipeline_inputs$code_dir
  configs <- pipeline_inputs$configs
  real_data_flag <- ifelse('loc_list' %in% names(pipeline_inputs)==T, T, F)
  
  source(file.path(code_dir,'simulations/1_simulate_data.R'))
  source(file.path(code_dir,'pipeline/1_prep_data.R'))
  source(file.path(code_dir,'pipeline/2_make_predictions.R'))
  source(file.path(code_dir,'pipeline/3_ensemble.R'))
  source(file.path(code_dir,'pipeline/4_adjust_UI.R'))
  
  ## PIPELINE 
  # (1) Simulate or prep data
  data_start <- Sys.time()
  if (real_data_flag){
    data <- prep_data(pipeline_inputs)
  } else {
    data <- simulate_data(param_set, pipeline_inputs)
  }
  data_end <- Sys.time()
  data_time <- data_end - data_start
  
  # (2) Make predictions
  pred_start <- Sys.time()
  preds_by_model <- make_predictions(data, pipeline_inputs)
  sigmas_dt <- unique(preds_by_model[,.(model, time_id, sigma)])
  pred_end <- Sys.time()
  pred_time <- pred_end - pred_start
  
  # (3) Ensemble
  ensemble_start <- Sys.time()
  ensemble_out_list <- ensemble(data, preds_by_model, pipeline_inputs)
  unadj_results <- ensemble_out_list$unadj_results
  unadj_results2 <- ensemble_out_list$unadj_results2
  weights_dt <- ensemble_out_list$weights
  fit_stats_dt <- ensemble_out_list$fit_stats
  ensemble_end <- Sys.time()
  ensemble_time <- ensemble_end - ensemble_start
  
  # (4) Adjust the UI
  adjust_start <- Sys.time()
  adjusted_results <- adjust_UI(data, unadj_results, unadj_results2, configs)
  final_results <- adjusted_results$final_results
  wis_results <- adjusted_results$final_results2
  adjust_end <- Sys.time()
  adjust_time <- adjust_end - adjust_start
  
  
  ## PREP RESULTS FOR OUTPUT
  draw_cols <- paste0('draw_', 1:configs$d)
  pi_probs <- c(0.01, 0.025, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45,
                0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975, 0.99)
  pi_names <- paste0('q', pi_probs * 100)
  
  # (1) Observations
  # data
  
  # (2) Candidate model estimates
  if(configs$save_candidate_draws==T){
    candidate_mod_output <- preds_by_model[, .SD, .SDcols = c('model','location_id', 'time_id', draw_cols)]
  } else {
    quantile_dt <- as.data.table(t(apply(preds_by_model[, .SD, .SDcols = draw_cols], 1, quantile, pi_probs)))
    setnames(quantile_dt, pi_names)
    quantile_dt[, mean := preds_by_model[, rowMeans(.SD), .SDcols = draw_cols]]
    candidate_mod_output <- cbind(preds_by_model[, .(model, location_id, time_id)], quantile_dt)
  }
  
  # (3) Pre-adjustment ensemble estimates
  forecast_target <- configs$w - 1
  
  # add p-value
  unadj_results <- merge(unadj_results, data, by=c('location_id', 'time_id'))
  unadj_results$p_val <- rowSums(unadj_results[, lapply(.SD, function(x) x <= y), .SDcols = draw_cols])/length(draw_cols)
  
  if(configs$save_pre_adj_draws==T){
    if(configs$save_all_pre_adj_time_steps==T){
      # save out draws all time steps
      pre_adj_output <- unadj_results[, .SD, .SDcols = c('location_id', 'time_id', 'p_val', draw_cols)]
    } else {
      # save out draws, only time step of interest
      pre_adj_output <- unadj_results[time_id==forecast_target,
                                      .SD, .SDcols = c('location_id', 'time_id', 'p_val', draw_cols)]
    }
  } else {
    if(configs$save_all_pre_adj_time_steps==T){
      # save out summary stats, all time steps
      quantile_dt <- as.data.table(t(apply(unadj_results[, .SD, .SDcols = draw_cols], 1, quantile, pi_probs)))
      setnames(quantile_dt, pi_names)
      quantile_dt[, mean := unadj_results[, rowMeans(.SD), .SDcols = draw_cols]]
      pre_adj_output <- cbind(unadj_results[, .(location_id, time_id, p_val)], quantile_dt)
    } else {
      # save out summary stats, only time step of interest
      tmp <- unadj_results[time_id==forecast_target]
      quantile_dt <- as.data.table(t(apply(tmp[, .SD, .SDcols = draw_cols], 1, quantile, pi_probs)))
      setnames(quantile_dt, pi_names)
      quantile_dt[, mean := tmp[, rowMeans(.SD), .SDcols = draw_cols]]
      pre_adj_output <- cbind(tmp[, .(location_id, time_id, p_val)], quantile_dt)
    }
  }
  
  # (4) & (5) Pre- and post-adjustment coverage rate
  coverage_pre <- adjusted_results$coverage_pre
  coverage_post <- adjusted_results$coverage_post
  
  # (6) & (7) Multipliers
  multiplier <- adjusted_results$multiplier   #final multiplier (t<0)
  multiplier2 <- adjusted_results$multiplier2 #multiplier for WIS (t<-1)

  # (8) Adjusted forecasts
  if(configs$save_draws==T){
    if(configs$save_all_time_steps==T){ 
      # save out draws, all time steps
      results_output <- final_results[, .SD, .SDcols = c('location_id', 'time_id','p_val', draw_cols)]
    } else{
      # save out draws, only time step of interest
      results_output <- final_results[time_id==forecast_target,
                                      .SD, .SDcols = c('location_id', 'time_id','p_val', draw_cols)]
    }
  } else { 
    if(configs$save_all_time_steps==T){ 
      # save out summary stats, all time steps
      results_output <- final_results[, `:=` (q1 = apply(.SD, 1, quantile, 0.01),
                                              q2.5 = apply(.SD, 1, quantile, 0.025),
                                              q50 = apply(.SD, 1, quantile, 0.50),
                                              mean = apply(.SD, 1, mean),
                                              q97.5 = apply(.SD, 1, quantile, 0.975)),
                                      .SDcols = draw_cols][,.(location_id, time_id, q1, q2.5, q50, mean, q97.5, p_val)]
    } else{
      # save out summary, only time step of interest
      results_output <- final_results[time_id==forecast_target][,
                                      `:=` (q1 = apply(.SD, 1, quantile, 0.01),
                                            q2.5 = apply(.SD, 1, quantile, 0.025),
                                            q50 = apply(.SD, 1, quantile, 0.50),
                                            mean = apply(.SD, 1, mean),
                                            q97.5 = apply(.SD, 1, quantile, 0.975)),
                                      .SDcols = draw_cols][,.(location_id, time_id, q1, q2.5, q50, mean, q97.5, p_val)]
    }
  }
  
  # (9) Adjusted forecasts for WIS (t=-1)
  #wis_results
  
  # (10) & (11) Ensemble weights and fit statistics
  # weights_dt
  # fit_stats_dt
  
  # (12) Sigmas
  # sigmas_dt
  
  # (13) Time stamps
  time_stamps <- c(data_time = data_time, pred_time = pred_time, ensemble_time = ensemble_time, adjust_time = adjust_time)
  
  return(list(obs_dt = data,
              candidate_mod_output = candidate_mod_output,
              pre_adj_output = pre_adj_output,
              coverage_pre = coverage_pre,
              coverage_post = coverage_post,
              multiplier = multiplier,
              multiplier2 = multiplier2,
              results_output = results_output,
              wis_results = wis_results,
              weights_dt = weights_dt,
              fit_stats_dt = fit_stats_dt,
              sigmas_dt = sigmas_dt,
              time_stamps = time_stamps))
}