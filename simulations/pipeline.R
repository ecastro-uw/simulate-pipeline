# pipeline

pipeline <- function(param_set, pipeline_inputs){
  
  code_dir <- pipeline_inputs$code_dir
  
  source(file.path(code_dir,'simulations/simulate_data.R'))
  source(file.path(code_dir,'simulations/fit_ensemble.R'))
  source(file.path(code_dir,'pipeline/adjust_UI.R'))
  
  ## PIPELINE 
  # (1) Simulate data
  sim_start <- Sys.time()
  sim_dat <- simulate_data(param_set)
  sim_end <- Sys.time()
  sim_time <- sim_end - sim_start
  
  # (2) Fit the model
  fit_start <- Sys.time()
  results_draws <- fit_ensemble(sim_dat, pipeline_inputs)
  fit_end <- Sys.time()
  fit_time <- fit_end - fit_start
  
  # (3) Adjust the UI
  adjust_start <- Sys.time()
  adjusted_results <- adjust_UI(results_draws, pipeline_inputs$problem_log, pipeline_inputs$configs)
  final_results <- adjusted_results$final_results
  adjust_end <- Sys.time()
  adjust_time <- adjust_end - adjust_start
  
  ## PREP RESULTS FOR OUTPUT
  
  # (1) Observations
  loc_id_list <- rep(1:param_set$L, each=param_set$t + pipeline_inputs$configs$w)
  obs_dt <- cbind(data.table(location_id=loc_id_list), rbindlist(lapply(final_results,
                             function(x) x$obs)))
  
  # (2) Pre-adjustment Forecasts
  forecast_target <- pipeline_inputs$configs$w - 1
  if(param_set$save_pre_adj_draws==T){
    if(param_set$save_all_pre_adj_time_steps==T){
      # draws all time steps
      pre_adj_output <- rbindlist(lapply(final_results, function(x) x$ensemble))
    } else {
      # draws, only time step of interest
      pre_adj_output <- rbindlist(lapply(final_results, function(x) x$ensemble[time_id==forecast_target]))
    }
  } else {
    if(param_set$save_all_pre_adj_time_steps==T){
      # summary, all time steps
      summarize <- function(results_one_loc){
        pred_tmp <- as.data.table(results_one_loc$ensemble)
        cols <- names(pred_tmp)[-1]
        
        dt <- data.table(
          time_id = pred_tmp$time_id,
          q1    = pred_tmp[, apply(.SD, 1, quantile, 0.010), .SDcols = cols],
          q2.5  = pred_tmp[, apply(.SD, 1, quantile, 0.025), .SDcols = cols],
          q50   = pred_tmp[, apply(.SD, 1, quantile, 0.500), .SDcols = cols],
          q97.5 = pred_tmp[, apply(.SD, 1, quantile, 0.975), .SDcols = cols]
        )
      }
    } else {
      # summary, only time step of interest
      summarize <- function(results_one_loc){
        pred_tmp <- as.data.table(results_one_loc$ensemble)[time_id==forecast_target]
        cols <- names(pred_tmp)[-1]
        
        dt <- data.table(
          time_id = pred_tmp$time_id,
          q1    = pred_tmp[, apply(.SD, 1, quantile, 0.010), .SDcols = cols],
          q2.5  = pred_tmp[, apply(.SD, 1, quantile, 0.025), .SDcols = cols],
          q50   = pred_tmp[, apply(.SD, 1, quantile, 0.500), .SDcols = cols],
          q97.5 = pred_tmp[, apply(.SD, 1, quantile, 0.975), .SDcols = cols]
        )
      }
    }
    pre_adj_output <- rbindlist(lapply(final_results, summarize))
  }
  
  # (3) Pre-adjustment coverage rate
  coverage_pre <- adjusted_results$coverage_pre
  
  # (4) Post-adjustment coverage rate
  coverage_post <- adjusted_results$coverage_post
  
  # (5) Multiplier
  multiplier <- adjusted_results$multiplier

  # (6) Adjusted forecasts
  if(param_set$save_draws==T){
    if(param_set$save_all_time_steps==T){ #TODO p-value should be included in draw-level results too (not just summary)
      # draws, all time steps
      results_output <- rbindlist(lapply(final_results, function(x) x$ensemble_adj))
    } else{
      # draws, only time step of interest
      results_output <- rbindlist(lapply(final_results, function(x) x$ensemble_adj[time_id==forecast_target]))
    }
  } else { 
    if(param_set$save_all_time_steps==T){ 
      # summary, all time steps
      summarize <- function(results_one_loc){
        # forecasts
        pred_tmp <- results_one_loc$ensemble_adj
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
        dt <- merge(dt, p_tmp, by='time_id', all=T)
      }
    } else{
      # summary, only time step of interest
      summarize <- function(results_one_loc){
        # forecasts
        pred_tmp <- results_one_loc$ensemble_adj[time_id==forecast_target]
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
        dt <- merge(dt, p_tmp, by='time_id', all.x=T)
      }
    }
    results_output <- rbindlist(lapply(final_results, summarize))
  }
  
  # (7) Time stamps
  time_stamps <- c(sim_time = sim_time, fit_time = fit_time, adjust_time = adjust_time)
  
  return(list(obs_dt = obs_dt,
              pre_adj_output = pre_adj_output,
              coverage_pre = coverage_pre,
              coverage_post = coverage_post,
              multiplier = multiplier,
              results_output = results_output,
              time_stamps = time_stamps))
}