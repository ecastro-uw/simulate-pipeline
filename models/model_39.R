# Model 39: Exponential Smoothing State Space (ETS) model, fit separately per location.
# Uses the forecast package's ets() function with automatic model selection across
# all ETS(E,T,S) families (error, trend, seasonal components).
# Forecasts w weeks ahead and returns d simulation draws per location.

model_39 <- function(dataset, w, d) {

  locations    <- unique(dataset$location_id)
  results_list <- vector("list", length(locations))

  for (i in seq_along(locations)) {
    loc      <- locations[i]
    loc_data <- dataset[location_id == loc]
    setorder(loc_data, time_id)

    # No frequency set (ts() defaults to 1) because seasonal structure
    # cannot be estimated from a short series.
    data_ts <- ts(loc_data$y)

    # Fit ETS with automatic model selection (information-criterion based).
    # restrict = FALSE allows multiplicative-error models even on series with
    # non-positive values, which ets() handles gracefully by falling back to
    # additive-error variants when needed.
    tmp_model <- ets(data_ts, restrict = FALSE)

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
      model       = "model_39",
      location_id = loc,
      time_id     = max(loc_data$time_id) + w,
      sigma       = sigma
    )
    results_list[[i]] <- cbind(ids, draws_dt)
  }

  return(rbindlist(results_list))
}
