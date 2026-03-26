pooled_nnet_model <- function(dataset, w, d){

  # Description of function:
  # Fits a single neural network (nnet) on lag features pooled across all locations.
  # This is a global forecasting model: all counties share one set of network weights,
  # but each county's own recent observations drive its forecast. Borrowing strength
  # across locations makes this useful when individual county series are short or noisy.
  #
  # Input y is log-normalized visits per 10K population (already in log space).
  # Forecasts w weeks ahead and returns d simulation draws per location.
  #
  # Model: y_t ~ f(y_{t-1}, y_{t-2})  [p=2 AR lags, shared across all locations]

  # -------------------------------------------------------------------------
  # (1) Build lagged input features for all locations
  # -------------------------------------------------------------------------
  dt <- copy(dataset)
  setorder(dt, location_id, time_id)
  dt[, y_lag1 := shift(y, 1L), by = location_id]
  dt[, y_lag2 := shift(y, 2L), by = location_id]

  # Training rows: drop time steps where either lag is missing
  train_dt <- dt[!is.na(y_lag2)]

  # -------------------------------------------------------------------------
  # (2) Fit one shared nnet on pooled training data
  #   linout = TRUE : linear output layer for unbounded continuous y
  #   size  = 5    : moderate hidden layer; pooling across L counties gives
  #                  ~6L training rows, which supports more capacity than
  #                  per-location fitting
  #   decay = 0.1  : L2 weight regularization to guard against overfitting
  #   maxit = 1000 : enough iterations for convergence on this problem size
  # Run `repeats` random starts and keep the network with the lowest
  # training RSS (same strategy as nnetar's repeats argument).
  # -------------------------------------------------------------------------
  repeats <- 20
  best_fit   <- NULL
  best_value <- Inf

  for (r in seq_len(repeats)) {
    fit <- nnet(y ~ y_lag1 + y_lag2, data = train_dt,
                size = 5, linout = TRUE, decay = 0.1,
                maxit = 1000, trace = FALSE)
    if (fit$value < best_value) {
      best_value <- fit$value
      best_fit   <- fit
    }
  }

  # -------------------------------------------------------------------------
  # (3) Compute sigma as pooled RMSE of in-sample residuals
  #     (globally pooled across all locations, matching naive_slope_2 style)
  # -------------------------------------------------------------------------
  fitted_vals <- as.numeric(predict(best_fit, train_dt))
  resids      <- train_dt$y - fitted_vals
  sigma       <- sqrt(mean(resids^2, na.rm = TRUE))

  # -------------------------------------------------------------------------
  # (4) Generate d draws per location for the w-step-ahead forecast
  #
  # For each draw we propagate noise forward w steps:
  #   - point forecast at each step = network prediction from current lags
  #   - add N(0, sigma) residual noise at each step
  #   - use the noisy draw as the lag input for the next step
  # This correctly accumulates uncertainty over multi-step horizons.
  # -------------------------------------------------------------------------

  # Last observed row per location (contains y_t and y_lag1 = y_{t-1})
  last_dt <- dt[, .SD[.N], by = location_id]

  locations   <- unique(dataset$location_id)
  results_list <- vector("list", length(locations))

  for (i in seq_along(locations)) {
    loc      <- locations[i]
    loc_last <- last_dt[location_id == loc]

    draws <- sapply(seq_len(d), function(x) {
      # cur  = y_t  (most recent observed value)
      # prev = y_{t-1}
      cur  <- loc_last$y
      prev <- loc_last$y_lag1

      val <- NA_real_
      for (step in seq_len(w)) {
        new_data <- data.frame(y_lag1 = cur, y_lag2 = prev)
        point    <- as.numeric(predict(best_fit, new_data))
        val      <- point + rnorm(1L, mean = 0, sd = sigma)
        prev     <- cur
        cur      <- val   # feed noisy draw forward as next lag
      }
      val
    })

    draws_dt <- as.data.table(t(draws))
    setnames(draws_dt, paste0("draw_", seq_len(d)))

    ids <- data.table(
      model       = "pooled_nnet_model",
      location_id = loc,
      time_id     = max(dataset[location_id == loc]$time_id) + w,
      sigma       = sigma
    )
    results_list[[i]] <- cbind(ids, draws_dt)
  }

  return(rbindlist(results_list))
}
