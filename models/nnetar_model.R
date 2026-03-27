nnetar_model <- function(dataset, w, d){

  # Description of function:
  # Neural network auto-regressive model, fit separately per location.
  # Input y is log-normalized visits per 10K population (already in log space).
  # Forecasts w weeks ahead and returns d simulation draws per location.

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

    # Fit nnetar:
    #   p=2   : use 2 lagged weeks as AR inputs (appropriate for 8-obs training window)
    #   P=0   : no seasonal lags (frequency=1, so seasonal structure is undefined)
    #   size=2: small hidden layer to avoid overfitting with 8 training points
    #   lambda=NULL: data is already log-normalized; no additional transformation needed
    #   repeats=20: enough ensemble networks for stable estimates on a short series
    tmp_model <- nnetar(data_ts, p = 2, P = 0, size = 2, repeats = 20, lambda = NULL)

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
      model       = "nnetar_model",
      location_id = loc,
      time_id     = max(loc_data$time_id) + w,
      sigma       = sigma
    )
    results_list[[i]] <- cbind(ids, draws_dt)
  }

  return(rbindlist(results_list))
}
