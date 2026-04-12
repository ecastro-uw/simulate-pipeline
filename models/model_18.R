# Model 18: auto.arima 
# Covariates: cases_lag2_sum (sum of cases_pc over previous 2 weeks)
#             deaths_lag2_sum (sum of deaths_pc over previous 2 weeks)

model_18 <- function(dataset, w, d) {

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

    data_ts    <- ts(loc_data_complete$y)
    xreg_train <- matrix(
      c(loc_data_complete$cases_lag2_sum, loc_data_complete$deaths_lag2_sum),
      ncol = 2,
      dimnames = list(NULL, c("cases_lag2_sum", "deaths_lag2_sum"))
    )

    tmp_model <- auto.arima(data_ts, xreg = xreg_train)

    # Build future xreg for the w forecast steps.
    # For each variable: lag2_sum at horizon s = var_pc[n+s-1] + var_pc[n+s-2].
    # When an index exceeds n, use persistence (last observed value).
    xreg_future <- matrix(NA_real_, nrow = w, ncol = 2,
                          dimnames = list(NULL, c("cases_lag2_sum", "deaths_lag2_sum")))
    for (s in seq_len(w)) {
      idx1 <- n + s - 1
      idx2 <- n + s - 2
      cases_val1  <- if (idx1 <= n) cases_observed[idx1]  else cases_observed[n]
      cases_val2  <- if (idx2 <= n) cases_observed[idx2]  else cases_observed[n]
      deaths_val1 <- if (idx1 <= n) deaths_observed[idx1] else deaths_observed[n]
      deaths_val2 <- if (idx2 <= n) deaths_observed[idx2] else deaths_observed[n]
      xreg_future[s, "cases_lag2_sum"]  <- cases_val1  + cases_val2
      xreg_future[s, "deaths_lag2_sum"] <- deaths_val1 + deaths_val2
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
      model       = "model_18",
      location_id = loc,
      time_id     = max(loc_data$time_id) + w,
      sigma       = sigma
    )
    results_list[[i]] <- cbind(ids, draws_dt)
  }

  return(rbindlist(results_list))
}
