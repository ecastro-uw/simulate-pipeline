# investigations/compare_pooled_vs_local_arima.R
#
# Question: does pooling the AR(1) coefficient across locations improve fit
# compared to estimating it separately per location?
#
# Approach: leave-last-out cross-validation on simulated AR(1) data.
#   - Simulate L locations, each with t observations from a true AR(1) process.
#   - Train on the first t-1 observations; evaluate one-step-ahead prediction
#     error at step t.
#   - Compare two estimators:
#       pooled : one phi estimated from all locations' stacked (y_t, y_{t-1}) pairs
#       local  : one phi estimated per location from that location's data only
#   - Repeat over 500 replications and summarise RMSE by scenario.
#
# Key factors expected to influence the comparison:
#   L   : more locations → pooled phi is estimated more precisely
#   t   : longer per-location series → local phi is estimated more precisely
#   phi : the true AR coefficient (doesn't affect relative precision directly,
#         but changes the signal-to-noise ratio)

library(data.table)

set.seed(42)

# -----------------------------------------------------------------------
# (1) Simulate AR(1) data for L locations over t time steps
# -----------------------------------------------------------------------
sim_ar1 <- function(L, t, phi, sigma = 0.2, y0 = 0) {
  dt_list <- vector("list", L)
  for (l in seq_len(L)) {
    y    <- numeric(t)
    y[1] <- y0
    for (j in 2:t) y[j] <- phi * y[j - 1] + rnorm(1L, 0, sigma)
    dt_list[[l]] <- data.table(location_id = l, time_id = seq_len(t), y = y)
  }
  rbindlist(dt_list)
}

# -----------------------------------------------------------------------
# (2) Leave-last-out RMSE for pooled vs. local AR(1)
#
# Train on time_id < max_t; predict at time_id == max_t.
# Both methods use the same AR(1) form:
#   yhat_t = intercept_l + phi * y_{t-1}
# They differ only in how phi is estimated:
#   pooled : phi from OLS on all locations' stacked pairs
#   local  : phi from OLS on each location's own pairs
# The per-location intercept is always estimated locally as the mean of
# (y - phi * lagged_y) over the training window, so both methods allow
# each location to have its own level.
# -----------------------------------------------------------------------
loo_rmse <- function(dt, method = c("pooled", "local")) {
  method <- match.arg(method)
  max_t  <- max(dt$time_id)

  train <- copy(dt[time_id < max_t])
  test  <- dt[time_id == max_t]

  train[, lagged_y := shift(y), by = location_id]
  train_complete <- train[!is.na(lagged_y)]

  if (method == "pooled") {
    phi_hat <- unname(coef(lm(y ~ lagged_y, data = train_complete))["lagged_y"])

    # Location-specific intercept; last observed y used to predict step t
    params <- train_complete[, .(
      intercept = mean(y - phi_hat * lagged_y),
      last_y    = y[.N]
    ), by = location_id]
    params[, yhat := intercept + phi_hat * last_y]

  } else {  # local
    params <- train_complete[, {
      fit <- lm(y ~ lagged_y)
      .(intercept = coef(fit)[1L],
        phi_hat   = coef(fit)[2L],
        last_y    = y[.N])
    }, by = location_id]
    params[, yhat := intercept + phi_hat * last_y]
  }

  merged <- merge(test, params[, .(location_id, yhat)], by = "location_id")
  sqrt(mean((merged$y - merged$yhat)^2))
}

# -----------------------------------------------------------------------
# (3) Scenario grid and replications
# -----------------------------------------------------------------------
# L : number of locations (= effective sample size for pooled phi)
# t : per-location training window (= effective sample size for local phi)
# phi : true AR coefficient
scenarios <- CJ(
  L   = c(5L, 15L, 50L),
  t   = c(5L, 8L, 12L),
  phi = c(0.3, 0.6, 0.9)
)

n_reps <- 500L

results <- scenarios[, {
  rmse_pooled <- numeric(n_reps)
  rmse_local  <- numeric(n_reps)
  for (r in seq_len(n_reps)) {
    dt              <- sim_ar1(L = L, t = t, phi = phi)
    rmse_pooled[r]  <- loo_rmse(dt, "pooled")
    rmse_local[r]   <- loo_rmse(dt, "local")
  }
  .(pooled_rmse = mean(rmse_pooled),
    local_rmse  = mean(rmse_local),
    pooled_wins = mean(rmse_pooled < rmse_local))
}, by = .(L, t, phi)]

setorder(results, phi, t, L)

# -----------------------------------------------------------------------
# (4) Print results
# -----------------------------------------------------------------------
results[, delta_pct := round(100 * (local_rmse - pooled_rmse) / local_rmse, 1)]
results[, pooled_wins_pct := round(100 * pooled_wins, 1)]

cat("\nLeave-last-out RMSE comparison: pooled vs. local AR(1) coefficient\n")
cat("sigma = 0.2 (fixed), 500 replications per scenario\n")
cat("delta_pct > 0 means pooled is better (local RMSE is higher)\n\n")

print(results[, .(L, t, phi,
                  pooled_rmse  = round(pooled_rmse, 4),
                  local_rmse   = round(local_rmse,  4),
                  delta_pct,
                  pooled_wins_pct)],
      row.names = FALSE)
