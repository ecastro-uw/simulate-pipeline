# Model 24: GLS with ARMA(1,1) errors
# Covariates: Cases & Deaths (total reported per 10K pop over last 2 weeks)
# Coefficients estimated globally across all locations.
# ARMA(1,1) error structure captures temporal autocorrelation.

model_24 <- function(dataset, w, d) {

  dt <- copy(dataset)
  setorder(dt, location_id, time_id)

  # Calculate 2-week rolling sums (excluding current week) for cases and deaths
  dt[, cases_lag2_sum  := frollsum(shift(cases_pc,  1), n = 2, align = "right"),
     by = location_id]
  dt[, deaths_lag2_sum := frollsum(shift(deaths_pc, 1), n = 2, align = "right"),
     by = location_id]

  # Drop rows with NA covariates (first 2 rows per location due to lagging)
  dt_complete <- dt[!is.na(cases_lag2_sum) & !is.na(deaths_lag2_sum)]

  # Fit gls with ARMA(1,1) errors, pooled across all locations
  fit <- gls(y ~ cases_lag2_sum + deaths_lag2_sum,
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

  # Build forecast data
  last_time_step <- max(dt$time_id)
  last_two <- dt[time_id %in% c(last_time_step - 1L, last_time_step),
                 .(location_id, time_id, cases_pc, deaths_pc)]
  new_dt <- last_two[, .(time_id         = last_time_step + w,
                         cases_lag2_sum  = sum(cases_pc),
                         deaths_lag2_sum = sum(deaths_pc)),
                     by = location_id]

  # Design matrix
  X_new <- model.matrix(~cases_lag2_sum + deaths_lag2_sum, data = new_dt)  # n_loc x 3

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

  ids <- data.table(model       = "model_24",
                    location_id = new_dt$location_id,
                    time_id     = new_dt$time_id,
                    sigma       = sigma)

  return(cbind(ids, draws_dt))
}
