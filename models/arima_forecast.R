arima_forecast <- function(dataset, w, d) {

  # Description of function:
  # ARIMA model fit separately per location using Arima() from the forecast package.
  # Input y is log-normalized visits per 10K population (already in log space).
  # Forecasts w weeks ahead and returns d simulation draws per location.
  # Model order is ARIMA(1,0,0) — an AR(1) — which is appropriate for short
  # training windows (< ~10 observations) where higher-order models are
  # likely to overfit.

  locations <- unique(dataset$location_id)

  results_list <- vector("list", length(locations))

  for (i in seq_along(locations)) {
    loc <- locations[i]
    loc_data <- dataset[location_id == loc]
    setorder(loc_data, time_id)

    # Build time series for this location.
    # No frequency set (ts() defaults to 1) because < 1 year of data and
    # within-county annual seasonality cannot be estimated from this window.
    data_ts <- ts(loc_data$y)

    # Fit ARIMA model using Arima() from the forecast package.
    # order = c(p, d, q): AR(1), no differencing, no MA term.
    # include.mean = TRUE keeps the intercept so the model can capture a
    # non-zero level in the series.
    tmp_model <- Arima(data_ts, order = c(1, 0, 0), include.mean = TRUE)

    # Generate d simulation draws for the w-week-ahead forecast.
    # Each simulate() call draws one stochastic path; we keep only step w.
    draws <- sapply(seq_len(d), function(x) {
      sim <- as.numeric(simulate(tmp_model, future = TRUE, nsim = w))
      sim[w]
    })

    # Compute sigma as RMSE of in-sample residuals
    resids <- residuals(tmp_model)
    sigma <- sqrt(mean(resids^2, na.rm = TRUE))

    # Assemble one-row-per-location result (wide by draw)
    draws_dt <- as.data.table(t(draws))
    setnames(draws_dt, paste0("draw_", seq_len(d)))

    ids <- data.table(
      model       = "arima_forecast",
      location_id = loc,
      time_id     = max(loc_data$time_id) + w,
      sigma       = sigma
    )
    results_list[[i]] <- cbind(ids, draws_dt)
  }

  return(rbindlist(results_list))
}
