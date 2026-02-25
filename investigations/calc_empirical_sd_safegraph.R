# Calculate the "empirical sd" statistic for SafeGraph data
# for all 50 US states

# Load packages
library(data.table)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# Define file paths
input_data_path <- '/ihme/scratch/users/ems2285/thesis/intermediate/visits_by_cnty_day_cat_v3.csv'
output_path     <- '/ihme/scratch/users/ems2285/thesis/intermediate/empirical_sd_by_state.csv'

# Load the location hierarchy
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)

# Get all US states
us_states <- hierarchy[parent_id == 102 & level==2, .(location_id, location_name)]

# Load full input data once
full_data <- fread(input_data_path)

# ---------------------------------------------------------------------------
# Function: calculate empirical_sd for a single state
# Returns a list: empirical_sd, n_counties_total, n_counties_incomplete
# ---------------------------------------------------------------------------
calc_empirical_sd <- function(state_location_id) {

  # Counties belonging to this state
  locs_dt <- hierarchy[parent_id == state_location_id, .(location_id, location_name)]
  n_total <- nrow(locs_dt)
  incomplete <- function(n_complete) list(empirical_sd = NA_real_, n_counties_total = n_total,
                                          n_counties_incomplete = n_total - n_complete)
  if (n_total == 0) return(incomplete(0))

  # Determine date of first mandate imposition by county
  events_dt <- full_data[location_id %in% locs_dt$location_id & top_category %like% 'Restaurants',
                         .(location_id, date = as.Date(date), mandate = dining_close)][order(location_id, date)]
  events_dt[, lag := shift(mandate, fill = 0), by = location_id]
  events_dt <- events_dt[mandate != lag]
  events_dt <- events_dt[, .SD[1], by = location_id]
  if (nrow(events_dt) == 0) return(incomplete(0))

  # Load mobility data up to the last mandate date in the state
  mobility_dt <- full_data[location_id %in% locs_dt$location_id & top_category %like% 'Restaurants',
                           .(location_id, date = as.Date(date), y = visits_per_10k,
                             mandate = dining_close)][date < max(events_dt$date)]

  # --- Baseline: first 8 weeks of 2020 (56 days per county) ---
  baseline_dt <- mobility_dt[date >= '2020-01-01' & date < '2020-02-26']
  complete_baseline <- baseline_dt[, .N, by = location_id][N == 56, location_id]
  if (length(complete_baseline) == 0) return(incomplete(0))

  baseline_dt <- baseline_dt[location_id %in% complete_baseline]
  baseline_dt[, week_id := rep(1:8, each = 7), by = location_id]
  baseline_weekly <- baseline_dt[, .(y = log(sum(y))), by = c('location_id', 'week_id')]
  baseline_weekly <- baseline_weekly[, .(mean_jan_feb = mean(y)), by = location_id]

  # --- Pre-mandate window: 4 weeks (28 days) before each county's mandate ---
  mobility_dt <- merge(mobility_dt, events_dt[, .(location_id, onset = date)])
  mobility_dt <- mobility_dt[date >= (onset - 28) & date < onset]

  # Keep only counties with complete pre-mandate window AND complete baseline
  complete_locs <- intersect(
    mobility_dt[, .N, by = location_id][N == 28, location_id],
    complete_baseline
  )
  if (length(complete_locs) == 0) return(incomplete(0))
  mobility_dt <- mobility_dt[location_id %in% complete_locs]

  # Summarize to weekly level and log-transform
  mobility_dt[, time_id := rep(-4:(-1), each = 7), by = location_id]
  weekly_dt <- mobility_dt[, .(y = log(sum(y))), by = c('location_id', 'time_id')]

  # Normalize by Jan-Feb baseline
  weekly_dt <- merge(weekly_dt, baseline_weekly, by = 'location_id')
  weekly_dt[, y_norm := y / mean_jan_feb]

  # Week-to-week first differences
  weekly_dt[, delta := y_norm - shift(y_norm, type = 'lag'), by = location_id]
  weekly_dt <- weekly_dt[!is.na(delta)]
  if (nrow(weekly_dt) == 0) return(incomplete(0))

  list(
    empirical_sd          = sd(weekly_dt$delta),
    n_counties_total      = n_total,
    n_counties_incomplete = n_total - length(complete_locs)
  )
}

# ---------------------------------------------------------------------------
# Run for all states
# ---------------------------------------------------------------------------
results <- rbindlist(lapply(seq_len(nrow(us_states)), function(i) {
  sid   <- us_states$location_id[i]
  sname <- us_states$location_name[i]
  message(sprintf("[%d/%d] %s", i, nrow(us_states), sname))
  res <- tryCatch(
    calc_empirical_sd(sid),
    error = function(e) {
      message(sprintf("  ERROR: %s", e$message))
      list(empirical_sd = NA_real_, n_counties_total = NA_integer_, n_counties_incomplete = NA_integer_)
    }
  )
  data.table(location_id = sid, location_name = sname, empirical_sd = res$empirical_sd,
             n_counties_total = res$n_counties_total, n_counties_incomplete = res$n_counties_incomplete)
}))

# Save results
fwrite(results, output_path)
message(sprintf("Done. Results for %d states saved to:\n  %s", nrow(results), output_path))


# Translate the empirical sd and number of counties to a minimum effect size
# e.g. Alabama has 67 counties and an SD of 0.017
# From the sim results, an SNR of 0.12 needs 75 locs to hit 80% power. An SNR of 0.13
# needs 40 locations. Since AL has 67 counties, the smallest SNR it can tolerate is 0.13.
# SNR = effect size / noise
# SNR * noise = effect size
# 0.13 * 0.017 = 0.00221
# The smallest detectable effect at 80% power is 0.002. The units are change in logged weekly visits 


# Make a map of the minimum effect size per state
