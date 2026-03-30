arima_model <- function(dataset, w, d) {

  # Description of function:
  # ARIMA(1,0,0) model with a shared AR(1) coefficient estimated from data
  # pooled across all locations.  Each location retains its own mean level
  # (intercept) and residual variance, so the model partially pools information:
  # the AR dynamics are borrowed across locations while local levels are kept
  # free.  This is analogous to how ar_simple_lm estimates a single pooled slope
  # and pooled_nnet_model fits shared network weights across locations.
  #
  # Pooling step : stack all (y_t, y_{t-1}) pairs and run one OLS regression to
  #               obtain a single phi estimate.
  # Per-location : Arima() is fit with phi fixed to the pooled value; the
  #               intercept (mean) and residual variance are estimated locally.
  # Uncertainty  : simulate() draws w-step stochastic paths from each fitted
  #               Arima object; only step w is retained.
  #
  # Forecasts w weeks ahead and returns d simulation draws per location.

  dt <- copy(dataset)
  setorder(dt, location_id, time_id)

  # -----------------------------------------------------------------------
  # (1) Estimate a shared AR(1) coefficient from pooled data
  # -----------------------------------------------------------------------
  # Build one-step lagged pairs for every location, then fit a single OLS
  # regression y ~ lagged_y on all stacked rows.  The slope is the pooled
  # phi; the intercept is discarded (each location will estimate its own).
  dt[, lagged_y := shift(y), by = location_id]
  train_dt <- dt[!is.na(lagged_y)]
  pooled_fit <- lm(y ~ lagged_y, data = train_dt)
  pooled_phi <- unname(coef(pooled_fit)["lagged_y"])

  # -----------------------------------------------------------------------
  # (2) Fit per-location Arima() with phi fixed to the pooled estimate
  # -----------------------------------------------------------------------
  # fixed = c(ar1, intercept): the first element fixes the AR(1) coefficient;
  # NA in the second position tells Arima() to estimate the mean freely.
  # simulate() will then use the location's own residual variance for draws.

  locations    <- unique(dt$location_id)
  results_list <- vector("list", length(locations))

  for (i in seq_along(locations)) {
    loc      <- locations[i]
    loc_data <- dt[location_id == loc]
    setorder(loc_data, time_id)

    data_ts <- ts(loc_data$y)

    tmp_model <- Arima(data_ts, order = c(1, 0, 0),
                       include.mean = TRUE,
                       fixed = c(pooled_phi, NA))

    # Generate d simulation draws for the w-week-ahead forecast.
    # Each simulate() call draws one stochastic path; we keep only step w.
    draws <- sapply(seq_len(d), function(x) {
      sim <- as.numeric(simulate(tmp_model, future = TRUE, nsim = w))
      sim[w]
    })

    # Compute sigma as RMSE of in-sample residuals
    resids <- residuals(tmp_model)
    sigma  <- sqrt(mean(resids^2, na.rm = TRUE))

    draws_dt <- as.data.table(t(draws))
    setnames(draws_dt, paste0("draw_", seq_len(d)))

    ids <- data.table(
      model       = "arima_model",
      location_id = loc,
      time_id     = max(loc_data$time_id) + w,
      sigma       = sigma
    )
    results_list[[i]] <- cbind(ids, draws_dt)
  }

  return(rbindlist(results_list))
}
