model_14 <- function(dataset, w, d) {

  # Description of function:
  # ARIMA(1,1,0) model with a shared AR(1) coefficient estimated from data
  # pooled across all locations.  The series is first-differenced once (d = 1)
  # before the AR(1) component is applied, so the model captures trend-like
  # non-stationarity while still borrowing AR dynamics across locations.
  # Each location retains its own residual variance; the AR coefficient is pooled.
  #
  # Pooling step : compute first differences within each location, stack all
  #               (diff_y_t, diff_y_{t-1}) pairs, and run one OLS regression
  #               to obtain a single pooled phi.  The intercept is discarded.
  # Per-location : Arima() is fit with phi fixed to the pooled value and no
  #               drift term (include.mean = FALSE); residual variance is local.
  # Uncertainty  : simulate() draws w-step stochastic paths from each fitted
  #               Arima object; only step w is retained.
  #
  # Forecasts w weeks ahead and returns d simulation draws per location.

  dt <- copy(dataset)
  setorder(dt, location_id, time_id)

  # -----------------------------------------------------------------------
  # (1) Estimate a shared AR(1) coefficient from pooled differenced data
  # -----------------------------------------------------------------------
  # Compute first differences within each location, then build one-step lagged
  # pairs of those differences.  A single OLS regression diff_y ~ lagged_diff_y
  # is fit on all stacked rows; the slope is the pooled phi and the intercept
  # is discarded (no drift in the ARIMA(1,1,0) specification).
  dt[, diff_y        := y - shift(y),      by = location_id]
  dt[, lagged_diff_y := shift(diff_y),     by = location_id]
  train_dt   <- dt[!is.na(diff_y) & !is.na(lagged_diff_y)]
  pooled_fit <- lm(diff_y ~ lagged_diff_y, data = train_dt)
  pooled_phi <- unname(coef(pooled_fit)["lagged_diff_y"])

  # -----------------------------------------------------------------------
  # (2) Fit per-location Arima() with phi fixed to the pooled estimate
  # -----------------------------------------------------------------------
  # order = c(1, 1, 0): one AR term, one round of differencing, no MA term.
  # include.mean = FALSE: no drift; fixed = c(ar1) pins phi to the pooled value.
  # simulate() uses the location's own residual variance for stochastic draws.

  locations    <- unique(dt$location_id)
  results_list <- vector("list", length(locations))

  for (i in seq_along(locations)) {
    loc      <- locations[i]
    loc_data <- dt[location_id == loc]
    setorder(loc_data, time_id)

    data_ts <- ts(loc_data$y)

    tmp_model <- Arima(data_ts, order = c(1, 1, 0),
                       include.mean = FALSE,
                       fixed = c(pooled_phi))

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
      model       = "model_14",
      location_id = loc,
      time_id     = max(loc_data$time_id) + w,
      sigma       = sigma
    )
    results_list[[i]] <- cbind(ids, draws_dt)
  }

  return(rbindlist(results_list))
}
