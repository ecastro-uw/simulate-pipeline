# Model 35: LME with ARMA(1,1) errors
# Covariates: Gathering restrictions (% of days in effect during the previous week)
# Fixed-effect coefficients estimated globally across all locations.
# Location-specific random intercepts capture between-location mean differences.
# ARMA(1,1) error structure captures temporal autocorrelation.

model_35 <- function(dataset, w, d) {

  dt <- copy(dataset)
  setorder(dt, location_id, time_id)

  # Lag gathering restrictions by one week
  dt[, lagged_gathering := shift(pct_gathering), by = location_id]

  # Drop rows with NA covariates (first row per location due to lagging)
  dt_complete <- dt[!is.na(lagged_gathering)]

  # Fit lme with location random intercepts and global ARMA(1,1) errors
  fit <- lme(y ~ lagged_gathering,
             random      = ~1 | location_id,
             correlation = corARMA(p = 1, q = 1, form = ~time_id | location_id),
             data        = dt_complete)

  # Extract fixed-effect parameters
  beta_hat  <- fixef(fit)
  vcov_beta <- vcov(fit)

  # Extract ARMA(1,1) parameters
  params <- coef(fit$modelStruct$corStruct, unconstrained = FALSE)
  phi    <- params[["Phi1"]]
  theta  <- params[["Theta1"]]
  sigma  <- fit$sigma

  # Extract location random intercepts
  re_dt <- data.table(location_id = as.integer(rownames(ranef(fit))),
                      rand_int    = ranef(fit)[["(Intercept)"]])

  # Compute last innovation per location via ARMA(1,1) filter on conditional
  # residuals (y - fixed effects - random intercept):
  # eta_t = eps_t - phi*eps_{t-1} - theta*eta_{t-1}
  dt_complete[, cond_resid := residuals(fit)]
  loc_stats <- dt_complete[, {
    eps     <- cond_resid
    n       <- .N
    eta     <- numeric(n)
    eta[1L] <- eps[1L]
    for (j in 2L:n) eta[j] <- eps[j] - phi * eps[j - 1L] - theta * eta[j - 1L]
    .(last_resid = eps[n], last_eta = eta[n])
  }, by = location_id]

  # Build forecast data: lagged_gathering at T+1 = pct_gathering at T
  last_time_step <- max(dt$time_id)
  new_dt <- dt[time_id == last_time_step, .(location_id, time_id, pct_gathering)]
  new_dt[, time_id := last_time_step + w]
  setnames(new_dt, "pct_gathering", "lagged_gathering")

  # Design matrix
  X_new <- model.matrix(~lagged_gathering, data = new_dt)  # n_loc x 2

  # Draw fixed-effect beta from multivariate normal (coefficient uncertainty)
  beta_draws <- mvrnorm(d, mu = beta_hat, Sigma = vcov_beta)
  if (!is.matrix(beta_draws)) beta_draws <- matrix(beta_draws, nrow = 1L)

  # Fixed-effect component: d x n_loc
  fitted_draws <- beta_draws %*% t(X_new)

  # Add location random intercepts (point estimates) to each draw
  rand_int_ord <- re_dt[new_dt[, .(location_id)], on = "location_id"]$rand_int
  fitted_draws <- sweep(fitted_draws, 2L, rand_int_ord, "+")

  # w-step-ahead AR+MA correction: phi^w * eps_T + phi^{w-1} * theta * eta_T
  loc_ord       <- loc_stats[new_dt[, .(location_id)], on = "location_id"]
  ar_correction <- phi^w * loc_ord$last_resid + phi^(w - 1L) * theta * loc_ord$last_eta

  # Predictive draws: d x n_loc -> transpose to n_loc x d
  noise    <- matrix(rnorm(d * nrow(new_dt), 0, sigma), nrow = d, ncol = nrow(new_dt))
  pred_mat <- t(sweep(fitted_draws + noise, 2L, ar_correction, "+"))

  draws_dt <- as.data.table(pred_mat)
  setnames(draws_dt, paste0("draw_", seq_len(d)))

  ids <- data.table(model       = "model_35",
                    location_id = new_dt$location_id,
                    time_id     = new_dt$time_id,
                    sigma       = sigma)

  return(cbind(ids, draws_dt))
}
