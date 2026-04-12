# Model 16: auto.arima with cases_lag2_sum external regressor
# Covariates: cases_lag2_sum (sum of cases_pc over previous 2 weeks)
# Extension of model_15 (auto.arima without covariates)

model_16 <- function(dataset, w, d) {

  dt <- copy(dataset)
  setorder(dt, location_id, time_id)

  # Compute cases_lag2_sum: cases_pc[t-1] + cases_pc[t-2]
  # Mirrors the covariate construction in model_4.R
  dt[, cases_lag2_sum := frollsum(shift(cases_pc, 1), n = 2, align = "right"), by = location_id]

  locations    <- unique(dt$location_id)
  results_list <- vector("list", length(locations))

  for (i in seq_along(locations)) {
    loc      <- locations[i]
    loc_data <- dt[location_id == loc]
    setorder(loc_data, time_id)

    n              <- nrow(loc_data)
    cases_observed <- loc_data$cases_pc

    # Remove rows where cases_lag2_sum is NA (first 2 rows due to lagging)
    loc_data_complete <- loc_data[!is.na(cases_lag2_sum)]

    data_ts    <- ts(loc_data_complete$y)
    xreg_train <- matrix(loc_data_complete$cases_lag2_sum, ncol = 1,
                         dimnames = list(NULL, "cases_lag2_sum"))

    tmp_model <- auto.arima(data_ts, xreg = xreg_train)

    # Build future xreg for the w forecast steps.
    # cases_lag2_sum at horizon s = cases_pc[n+s-1] + cases_pc[n+s-2].
    # When an index exceeds n, use persistence (last observed value).
    xreg_future <- matrix(NA_real_, nrow = w, ncol = 1,
                          dimnames = list(NULL, "cases_lag2_sum"))
    for (s in seq_len(w)) {
      idx1 <- n + s - 1
      idx2 <- n + s - 2
      val1 <- if (idx1 <= n) cases_observed[idx1] else cases_observed[n]
      val2 <- if (idx2 <= n) cases_observed[idx2] else cases_observed[n]
      xreg_future[s, 1] <- val1 + val2
    }

    # Generate d simulation draws for the w-week-ahead forecast.
    # Each simulate() call draws one stochastic path; we keep only step w.
    draws <- sapply(seq_len(d), function(x) {
      sim <- as.numeric(simulate(tmp_model, future = TRUE, nsim = w, xreg = xreg_future))
      sim[w]
    })

    # Compute sigma as RMSE of in-sample residuals
    resids <- residuals(tmp_model)
    sigma  <- sqrt(mean(resids^2, na.rm = TRUE))

    draws_dt <- as.data.table(t(draws))
    setnames(draws_dt, paste0("draw_", seq_len(d)))

    ids <- data.table(
      model       = "model_16",
      location_id = loc,
      time_id     = max(loc_data$time_id) + w,
      sigma       = sigma
    )
    results_list[[i]] <- cbind(ids, draws_dt)
  }

  return(rbindlist(results_list))
}
