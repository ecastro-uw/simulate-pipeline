# Model 15: auto.arima 
# Covariates: cases_lag2_sum (sum of cases_pc over previous 2 weeks)
#             deaths_lag2_sum (sum of deaths_pc over previous 2 weeks)

model_15 <- function(dataset, w, d) {

  dt <- copy(dataset)
  setorder(dt, location_id, time_id)

  # Compute covariates: var_pc[t-1] + var_pc[t-2] for cases and deaths.
  dt[, cases_lag2_sum  := frollsum(shift(cases_pc,  1), n = 2, align = "right"), by = location_id]
  dt[, deaths_lag2_sum := frollsum(shift(deaths_pc, 1), n = 2, align = "right"), by = location_id]

  locations    <- unique(dt$location_id)
  results_list <- vector("list", length(locations))

  for (i in seq_along(locations)) {
    loc      <- locations[i]
    loc_data <- dt[location_id == loc]
    setorder(loc_data, time_id)

    n               <- nrow(loc_data)
    cases_observed  <- loc_data$cases_pc
    deaths_observed <- loc_data$deaths_pc

    # Remove rows where either covariate is NA (first 2 rows due to lagging)
    loc_data_complete <- loc_data[!is.na(cases_lag2_sum) & !is.na(deaths_lag2_sum)]

    # Only include regressors that have variation; a constant (e.g. all-zero)
    # regressor causes auto.arima to fail.
    use_cases  <- length(unique(loc_data_complete$cases_lag2_sum))  > 1
    use_deaths <- length(unique(loc_data_complete$deaths_lag2_sum)) > 1
    xreg_cols  <- c(
      if (use_cases)  "cases_lag2_sum",
      if (use_deaths) "deaths_lag2_sum"
    )

    data_ts <- ts(loc_data_complete$y)

    if (length(xreg_cols) > 0) {
      xreg_train <- as.matrix(loc_data_complete[, xreg_cols, with = FALSE])
      tmp_model  <- auto.arima(data_ts, xreg = xreg_train)
    } else {
      xreg_train <- NULL
      tmp_model  <- auto.arima(data_ts)
    }

    # When auto.arima selects a model with drift, it prepends a "drift" column
    # (1:nobs) to xreg internally. We must continue that trend in xreg_future.
    has_drift <- !is.null(tmp_model$xreg) && "drift" %in% colnames(tmp_model$xreg)
    n_train   <- nrow(loc_data_complete)

    # Build future xreg for the w forecast steps.
    # For each variable: lag2_sum at horizon s = var_pc[n+s-1] + var_pc[n+s-2].
    # When an index exceeds n, use persistence (last observed value).
    if (length(xreg_cols) > 0) {
      xreg_future <- matrix(NA_real_, nrow = w, ncol = length(xreg_cols),
                            dimnames = list(NULL, xreg_cols))
      for (s in seq_len(w)) {
        idx1 <- n + s - 1
        idx2 <- n + s - 2
        if (use_cases) {
          cases_val1 <- if (idx1 <= n) cases_observed[idx1] else cases_observed[n]
          cases_val2 <- if (idx2 <= n) cases_observed[idx2] else cases_observed[n]
          xreg_future[s, "cases_lag2_sum"] <- cases_val1 + cases_val2
        }
        if (use_deaths) {
          deaths_val1 <- if (idx1 <= n) deaths_observed[idx1] else deaths_observed[n]
          deaths_val2 <- if (idx2 <= n) deaths_observed[idx2] else deaths_observed[n]
          xreg_future[s, "deaths_lag2_sum"] <- deaths_val1 + deaths_val2
        }
      }
    } else {
      xreg_future <- NULL
    }

    if (has_drift) {
      drift_future <- matrix(seq(n_train + 1, n_train + w), nrow = w, ncol = 1,
                             dimnames = list(NULL, "drift"))
      xreg_future <- if (!is.null(xreg_future)) cbind(drift_future, xreg_future) else drift_future
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
      model       = "model_15",
      location_id = loc,
      time_id     = max(loc_data$time_id) + w,
      sigma       = sigma
    )
    results_list[[i]] <- cbind(ids, draws_dt)
  }

  return(rbindlist(results_list))
}
