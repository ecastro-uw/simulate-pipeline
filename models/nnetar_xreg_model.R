nnetar_xreg_model <- function(dataset, w, d){

  # Description of function:
  # Neural network autoregression model (nnetar) with an external regressor,
  # fit separately per location. Extends nnetar_model by including state-wide
  # COVID cases per 10K population (cases_pc) as an additional input to the
  # network alongside the p=2 AR lags of y.
  #
  # Input y is log-normalized visits per 10K population (already in log space).
  # Forecasts w weeks ahead and returns d simulation draws per location.
  #
  # NOTE on future xreg: nnetar's simulate() requires cases_pc values at each
  # forecast step, but those are not observed yet. We assume persistence --
  # the last observed cases_pc is carried forward for all w forecast steps.
  # This is most reasonable when w=1 and cases are slowly varying.

  locations    <- unique(dataset$location_id)
  results_list <- vector("list", length(locations))

  for (i in seq_along(locations)) {
    loc      <- locations[i]
    loc_data <- dataset[location_id == loc]
    setorder(loc_data, time_id)

    # Outcome time series
    data_ts <- ts(loc_data$y)

    # xreg for training: cases_pc column aligned row-for-row with data_ts
    xreg_train <- matrix(loc_data$cases_pc, ncol = 1,
                         dimnames = list(NULL, "cases_pc"))

    # Fit nnetar with external regressor:
    #   p=2        : 2 AR lags of y (same rationale as nnetar_model)
    #   P=0        : no seasonal lags (frequency=1)
    #   size=2     : small hidden layer; 3 total inputs (y_lag1, y_lag2, cases_pc)
    #                with only 8 training obs, keep capacity low to avoid overfitting
    #   lambda=NULL: y is already log-normalized
    #   repeats=20 : multiple random starts for stability on short series
    tmp_model <- nnetar(data_ts, p = 2, P = 0, size = 2, repeats = 20,
                        lambda = NULL, xreg = xreg_train)

    # Future xreg: carry the last observed cases_pc forward for all w steps
    # (persistence assumption -- see NOTE above)
    xreg_future <- matrix(rep(tail(loc_data$cases_pc, 1L), w), nrow = w, ncol = 1,
                          dimnames = list(NULL, "cases_pc"))

    # Generate d simulation draws for the w-step-ahead forecast.
    # Each simulate() call draws one stochastic path; we keep only step w.
    draws <- sapply(seq_len(d), function(x) {
      sim <- as.numeric(simulate(tmp_model, future = TRUE, nsim = w,
                                 xreg = xreg_future))
      sim[w]
    })

    # Compute sigma as RMSE of in-sample residuals
    resids <- residuals(tmp_model)
    sigma  <- sqrt(mean(resids^2, na.rm = TRUE))

    # Assemble one-row-per-location result (wide by draw)
    draws_dt <- as.data.table(t(draws))
    setnames(draws_dt, paste0("draw_", seq_len(d)))

    ids <- data.table(
      model       = "nnetar_xreg_model",
      location_id = loc,
      time_id     = max(loc_data$time_id) + w,
      sigma       = sigma
    )
    results_list[[i]] <- cbind(ids, draws_dt)
  }

  return(rbindlist(results_list))
}
