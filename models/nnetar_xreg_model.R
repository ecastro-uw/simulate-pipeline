nnetar_xreg_model <- function(dataset, w, d, p_x = 2){

  # Description of function:
  # Neural network autoregression model (nnetar) with lagged external regressors,
  # fit separately per location. Extends nnetar_model by including p_x lags of
  # state-wide COVID cases per 10K population (cases_pc) as additional inputs,
  # reflecting the hypothesis that reported case counts over the prior p_x weeks
  # influence current restaurant visit behavior.
  #
  # Input y is log-normalized visits per 10K population (already in log space).
  # Forecasts w weeks ahead and returns d simulation draws per location.
  #
  # Model inputs at time t:
  #   AR lags of y   : y_{t-1}, y_{t-2}          (p=2, handled by nnetar)
  #   xreg lags      : cases_{t-1}, ..., cases_{t-p_x}  (p_x columns, built manually)
  #
  # NOTE on future xreg: to predict y_{t+s}, lag k of cases requires
  # cases_{t+s-k}. For s=1 (one-step-ahead), all needed values are already
  # observed (indices t through t-p_x+1). Persistence is only needed when
  # w > 1 and s > k, i.e., when the forecast horizon exceeds the lag depth.

  locations    <- unique(dataset$location_id)
  results_list <- vector("list", length(locations))

  for (i in seq_along(locations)) {
    loc      <- locations[i]
    loc_data <- dataset[location_id == loc]
    setorder(loc_data, time_id)

    n              <- nrow(loc_data)
    cases_observed <- loc_data$cases_pc

    # -------------------------------------------------------------------------
    # (1) Build lagged xreg matrix for training
    #     Column k contains cases_pc lagged by k weeks: cases_{t-k}
    #     The first p_x rows will have NAs; trim both y and xreg to match.
    # -------------------------------------------------------------------------
    xreg_full <- matrix(NA_real_, nrow = n, ncol = p_x,
                        dimnames = list(NULL, paste0("cases_lag", seq_len(p_x))))
    for (k in seq_len(p_x)) {
      xreg_full[(k + 1):n, k] <- cases_observed[seq_len(n - k)]
    }

    # Trim the first p_x rows so nnetar trains on complete-case rows only
    xreg_train <- xreg_full[(p_x + 1):n, , drop = FALSE]
    data_ts    <- ts(loc_data$y[(p_x + 1):n])

    # -------------------------------------------------------------------------
    # (2) Fit nnetar with lagged cases as external regressors
    #   p=2        : 2 AR lags of y
    #   P=0        : no seasonal lags (frequency=1)
    #   size=2     : small hidden layer; (2 + p_x) total xreg inputs plus p AR
    #                lags; keep capacity low relative to trimmed training window
    #   lambda=NULL: y is already log-normalized
    #   repeats=20 : multiple random starts for stability on short series
    # -------------------------------------------------------------------------
    tmp_model <- nnetar(data_ts, p = 2, P = 0, size = 2, repeats = 20,
                        lambda = NULL, xreg = xreg_train)

    # -------------------------------------------------------------------------
    # (3) Build future xreg for the w forecast steps
    #
    # At forecast step s (predicting y_{n+s}), lag k requires cases_{n+s-k}.
    # Index in cases_observed = n + s - k (1-based).
    #   - If index <= n : value is observed; use it directly.
    #   - If index >  n : value is in the future; use persistence (cases_n).
    #
    # For w=1: s=1, index = n+1-k. Since k >= 1, index <= n for all k >= 1.
    # All values are observed -- no persistence assumption needed.
    # -------------------------------------------------------------------------
    xreg_future <- matrix(NA_real_, nrow = w, ncol = p_x,
                          dimnames = list(NULL, paste0("cases_lag", seq_len(p_x))))
    for (s in seq_len(w)) {
      for (k in seq_len(p_x)) {
        obs_idx <- n + s - k
        xreg_future[s, k] <- if (obs_idx <= n) cases_observed[obs_idx] else cases_observed[n]
      }
    }

    # -------------------------------------------------------------------------
    # (4) Generate d simulation draws for the w-step-ahead forecast
    # -------------------------------------------------------------------------
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
