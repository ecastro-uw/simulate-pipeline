# Ensemble
# Ensemble candidate models to generate predictions with uncertainty by location and time step

ensemble <- function(obs_dt, preds_dt, pipeline_inputs){

  source(file.path(pipeline_inputs$code_dir, "pipeline/model_performance_measures.R"))

  # Set some values
  configs <- pipeline_inputs$configs
  d <- configs$d
  list_of_models <- configs$models
  n_models <- length(list_of_models)

  # Inner function to run one ensemble iteration given a forecasts subset for weight optimization
  run_one_ensemble <- function(forecasts_subset) {

    if (n_models > 1){

      perform_func <- ifelse(configs$perform_meas=='MSE', calc_mse, calc_wis)
      vals <- logit(1/(n_models:2))

      if (length(vals)==1){
        fit <- optimize(f = perform_func,
                        interval = c(-9,9),
                        forecasts = forecasts_subset,
                        observations = obs_dt,
                        list_of_models = list_of_models,
                        d = d)
        weights <- create_weights(fit$minimum)
        fit_stats_dt <- data.table(
          convergence    = NA_integer_,
          objective_value = fit$objective,
          n_function_evals = NA_integer_,
          message        = NA_character_
        )
      } else {
        optim_method <- ifelse(configs$perform_meas == 'MSE', 'L-BFGS-B', 'Nelder-Mead')
        maxit <- 5000*(length(vals)+1)
        fit <- optim(par = vals,
                     fn = perform_func,
                     forecasts = forecasts_subset,
                     observations = obs_dt,
                     list_of_models = list_of_models,
                     d=d,
                     method = optim_method,
                     control = list(maxit = maxit))
        
        # If optim fails, re-try with different starting point
        n_restarts <- 3
        restart_attempt <- 0
        while (fit$convergence == 10 && restart_attempt < n_restarts) {
          restart_attempt <- restart_attempt + 1
          new_start_vals <- vals + rnorm(length(vals), sd = 1)
          fit_new <- optim(par = new_start_vals,
                           fn = perform_func,
                           forecasts = forecasts_subset,
                           observations = obs_dt,
                           list_of_models = list_of_models,
                           d=d,
                           method = optim_method,
                           control = list(maxit = maxit))
          if (fit_new$value < fit$value) fit <- fit_new
        }
        
        weights <- create_weights(fit$par)
        fit_stats_dt <- data.table(
          convergence      = fit$convergence,
          objective_value  = fit$value,
          n_function_evals = fit$counts[['function']],
          message          = ifelse(is.null(fit$message), NA_character_, fit$message)
        )
      }

      # Take a weighted average of model preds to get the ensemble preds.
      weights_dt <- data.table(model = list_of_models, weight = weights)
      preds_merged <- merge(preds_dt, weights_dt, by='model')

      # Calculate weighted average of each draw by location and time
      result <- preds_merged[, lapply(.SD, function(x) sum(x * weight)),
                             .SDcols = paste0("draw_",1:d), by=.(location_id, time_id)]
      result$model <- 'ensemble'
      setcolorder(result, "model")

    } else {
      # use results from the only candidate model
      result <- copy(preds_dt)[, model := 'ensemble']
      weights_dt <- data.table(model = list_of_models, weight = 1)
      fit_stats_dt <- data.table(
        convergence      = NA_integer_,
        objective_value  = NA_real_,
        n_function_evals = NA_integer_,
        message          = 'single model, no optimization'
      )
    }

    return(list(result = result, weights_dt = weights_dt, fit_stats_dt = fit_stats_dt))
  }

  # First ensemble: optimize weights on time_id %in% c(-2, -1)
  run1 <- run_one_ensemble(preds_dt[time_id %in% c(-2,-1)])

  # Second ensemble: optimize weights on time_id %in% c(-3, -2)
  run2 <- run_one_ensemble(preds_dt[time_id %in% c(-3,-2)])

  return(list(unadj_results  = run1$result[time_id>-3],
              weights        = run1$weights_dt,
              fit_stats      = run1$fit_stats_dt,
              unadj_results2 = run2$result[time_id<0]))
}
