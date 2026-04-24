# Model 29: GLS with ARMA(1,1) errors
# Covariates: Sum of mandates (edu + gather + gym + bar; % of days during the previous week)
# Coefficients estimated globally across all locations.
# ARMA(1,1) error structure captures temporal autocorrelation.

model_29 <- function(dataset, w, d) {

  dt <- copy(dataset)
  setorder(dt, location_id, time_id)

  # Calculate composite mandate score and lag by one week
  dt[, mandate_tot         := pct_edu + pct_gathering + pct_gym + pct_bar]
  dt[, lagged_mandate_tot  := shift(mandate_tot), by = location_id]

  # Drop rows with NA covariates (first row per location due to lagging)
  dt_complete <- dt[!is.na(lagged_mandate_tot)]

  # Fit gls with ARMA(1,1) errors, pooled across all locations
  fit <- gls(y ~ lagged_mandate_tot,
             data        = dt_complete,
             correlation = corARMA(p = 1, q = 1, form = ~time_id | location_id))

  # Extract ARMA(1,1) parameters
  params <- coef(fit$modelStruct$corStruct, unconstrained = FALSE)
  phi    <- params[["Phi1"]]
  theta  <- params[["Theta1"]]
  sigma  <- fit$sigma

  # Compute last innovation per location via ARMA(1,1) filter:
  # eta_t = eps_t - phi*eps_{t-1} - theta*eta_{t-1}
  dt_complete[, gls_resid := residuals(fit)]
  loc_stats <- dt_complete[, {
    eps     <- gls_resid
    n       <- .N
    eta     <- numeric(n)
    eta[1L] <- eps[1L]
    for (j in 2L:n) eta[j] <- eps[j] - phi * eps[j - 1L] - theta * eta[j - 1L]
    .(last_resid = eps[n], last_eta = eta[n])
  }, by = location_id]

  # Build forecast data: lagged_mandate_tot at T+1 = mandate_tot at T
  last_time_step <- max(dt$time_id)
  new_dt <- dt[time_id == last_time_step, .(location_id, time_id, mandate_tot)]
  new_dt[, time_id := last_time_step + w]
  setnames(new_dt, "mandate_tot", "lagged_mandate_tot")

  # Design matrix
  X_new <- model.matrix(~lagged_mandate_tot, data = new_dt)  # n_loc x 2

  # w-step-ahead AR+MA correction: phi^w * eps_T + phi^{w-1} * theta * eta_T
  loc_ord       <- loc_stats[new_dt[, .(location_id)], on = "location_id"]
  ar_correction <- phi^w * loc_ord$last_resid + phi^(w - 1L) * theta * loc_ord$last_eta

  # Draw beta from multivariate normal (coefficient uncertainty)
  beta_draws <- mvrnorm(d, mu = coef(fit), Sigma = vcov(fit))
  if (!is.matrix(beta_draws)) beta_draws <- matrix(beta_draws, nrow = 1L)

  # Predictive draws: d x n_loc -> transpose to n_loc x d
  fitted_draws <- beta_draws %*% t(X_new)
  noise        <- matrix(rnorm(d * nrow(new_dt), 0, sigma), nrow = d, ncol = nrow(new_dt))
  pred_mat     <- t(sweep(fitted_draws + noise, 2L, ar_correction, "+"))

  draws_dt <- as.data.table(pred_mat)
  setnames(draws_dt, paste0("draw_", seq_len(d)))

  ids <- data.table(model       = "model_29",
                    location_id = new_dt$location_id,
                    time_id     = new_dt$time_id,
                    sigma       = sigma)

  return(cbind(ids, draws_dt))
}
