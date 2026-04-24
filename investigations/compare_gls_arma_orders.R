# investigations/compare_gls_arma_orders.R
#
# Question: For the gls() model family (models 21-29), which ARMA error
# structure — AR(1), AR(2), or ARMA(1,1) — best fits the pooled data?
#
# Approach: Fit gls(y ~ 1) under three candidate correlation structures on
# pooled data and compare AIC and BIC.
#
# To run on real data: replace the "Simulate data" block with your own data
# loading code. The result should be a data.table with columns:
#   location_id  integer
#   time_id      integer, consecutive and ordered within each location
#   y            numeric
#
# To run on simulated data: adjust L, t, phi, theta, sigma below.

library(data.table)
library(nlme)

set.seed(42)

# -----------------------------------------------------------------------
# OPTION A: Simulate data
# -----------------------------------------------------------------------
L     <- 40    # number of locations
t     <- 8     # time steps per location
phi   <- 0.5   # true AR coefficient
theta <- 0.3   # true MA coefficient (0 = pure AR)
sigma <- 0.3   # innovation SD
mu    <- 0.0   # global mean

simulate_arma11 <- function(L, t, phi, theta, sigma, mu) {
  dt_list <- vector("list", L)
  for (l in seq_len(L)) {
    eps <- numeric(t)
    eta <- numeric(t)
    eta[1L] <- rnorm(1L, 0, sigma)
    eps[1L] <- eta[1L]
    for (j in 2L:t) {
      eta[j] <- rnorm(1L, 0, sigma)
      eps[j] <- phi * eps[j - 1L] + eta[j] + theta * eta[j - 1L]
    }
    dt_list[[l]] <- data.table(location_id = l, time_id = seq_len(t), y = mu + eps)
  }
  rbindlist(dt_list)
}

dt <- simulate_arma11(L, t, phi, theta, sigma, mu)

# -----------------------------------------------------------------------
# OPTION B: Real data — uncomment and replace the block above
# -----------------------------------------------------------------------
# source("pipeline/1_prep_data.R")
# dt <- prep_data(pipeline_inputs)    # standard pipeline dataset
# dt <- dt[, .(location_id, time_id, y)]   # keep only what's needed
# setorder(dt, location_id, time_id)

# -----------------------------------------------------------------------
# Fit candidate models
# -----------------------------------------------------------------------
cat("Fitting AR(1)...\n")
fit_ar1 <- gls(y ~ 1, data = dt,
               correlation = corARMA(p = 1, q = 0, form = ~time_id | location_id))

cat("Fitting AR(2)...\n")
# AR(2) is prone to "Coefficient matrix not invertible" errors because the
# stationarity region for (phi1, phi2) is a triangle in 2D; the optimizer
# can step outside it during initialisation. We try explicit starting values
# first, then fall back to NA if the model still fails to converge.
fit_ar2 <- tryCatch(
  gls(y ~ 1, data = dt,
      correlation = corARMA(p = 2, q = 0, value = c(0.3, 0.1),
                            form = ~time_id | location_id)),
  error = function(e) {
    cat("  AR(2) failed to converge:", conditionMessage(e), "\n")
    NULL
  }
)

cat("Fitting ARMA(1,1)...\n")
fit_arma11 <- gls(y ~ 1, data = dt,
                  correlation = corARMA(p = 1, q = 1, form = ~time_id | location_id))

# -----------------------------------------------------------------------
# Compare AIC / BIC
# -----------------------------------------------------------------------
safe_aic <- function(fit) if (is.null(fit)) NA_real_ else AIC(fit)
safe_bic <- function(fit) if (is.null(fit)) NA_real_ else BIC(fit)

results <- data.table(
  model    = c("AR(1)", "AR(2)", "ARMA(1,1)"),
  n_params = c(3L, 4L, 4L),   # intercept + ARMA params + sigma
  AIC      = c(safe_aic(fit_ar1), safe_aic(fit_ar2), safe_aic(fit_arma11)),
  BIC      = c(safe_bic(fit_ar1), safe_bic(fit_ar2), safe_bic(fit_arma11))
)
results[, delta_AIC := round(AIC - min(AIC, na.rm = TRUE), 2)]
results[, delta_BIC := round(BIC - min(BIC, na.rm = TRUE), 2)]
setorder(results, AIC, na.last = TRUE)

cat("\n=== ARMA order comparison (lower = better) ===\n")
print(results)

# -----------------------------------------------------------------------
# Print estimated parameters for each model
# -----------------------------------------------------------------------
cat("\n=== Estimated parameters ===\n")

for (nm in c("AR(1)", "AR(2)", "ARMA(1,1)")) {
  fit <- switch(nm, "AR(1)" = fit_ar1, "AR(2)" = fit_ar2, "ARMA(1,1)" = fit_arma11)
  cat(sprintf("\n%s:\n", nm))
  if (is.null(fit)) {
    cat("  (did not converge)\n")
    next
  }
  cat(sprintf("  intercept = %7.4f\n", coef(fit)[[1L]]))
  cat(sprintf("  sigma     = %7.4f\n", fit$sigma))
  pars <- coef(fit$modelStruct$corStruct, unconstrained = FALSE)
  for (i in seq_along(pars)) {
    cat(sprintf("  %-8s = %7.4f\n", names(pars)[i], pars[i]))
  }
}

# -----------------------------------------------------------------------
# Guidance
# -----------------------------------------------------------------------
cat("\n=== How to interpret ===\n")
cat("  delta_AIC < 2  : models are essentially equivalent\n")
cat("  delta_AIC 2-7  : moderate evidence for the better model\n")
cat("  delta_AIC > 10 : strong evidence for the better model\n")
cat("  BIC applies a stronger penalty for extra parameters (n_params).\n")
cat("  Prefer the model with the lowest AIC/BIC unless delta is < 2.\n")
