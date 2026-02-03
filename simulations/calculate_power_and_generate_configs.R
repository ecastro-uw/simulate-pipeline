# Find the smallest value of L that yields >= 80% power
# Approach:
# 1. Load results and calculate power by L and signal-to-noise ratio
# 2. Find the smallest L where power exceeds 80% (L*)
# 3. If L was run in "bins" of 1, this is your final answer
# 4. If L was run in bins of 10, generate a YAML file to re-run
#    for values of L=[L*-10, L*-9,...,L*]. Then repeat steps 1-3.

library(data.table)
library(stringr)
library(yaml)

# Which run version id do you want to examine?
version_id <- '20260203.01'
# Do you want to look at pre- or post-adjusted ensemble predictions? ('pre' or 'adj' are valid options)
pred_type <- 'adj'

# directory
root <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id)
dir <- file.path(root,'batched_output')

# parameters
params <- fread(file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id, 'params.csv'))

# define function for processing each file
load_file <- function(file){
  # define ids
  param <- as.numeric(str_extract(file, "(?<=_p)\\d+"))
  batch <- as.numeric(str_extract(file, "(?<=_b)\\d+"))
  n_draws <- params[param_id==param, d]

  # load data
  dt <- fread(file)[time_id==0, .(rep_id, location_id, time_id, p_val)]

  # tally up number of stat sig locs per rep
  dt[, verdict := ifelse(p_val<0.05,1,0)]
  rep_dt <- dt[, list(k = sum(verdict)), by=rep_id]
  rep_dt[, param_id := param]
  rep_dt[, batch_id :=  batch]
  rep_dt <- rep_dt[,.(param_id, batch_id, rep_id, k)]
}

# combine reps and calculate p-value/verdict for each rep
list_of_files <- list.files(dir, pattern=paste0("pred_",pred_type), full.names = T)
files_all <- rbindlist(lapply(list_of_files, load_file))
files_all <- merge(files_all, params[,.(param_id, L, signal_noise_ratio)], by='param_id')
files_all[, p_val := pbinom(k,L,0.05,lower.tail=F)]
files_all[, reject_null := ifelse(p_val <= 0.05, 1, 0)]

# calculate power for each parameter configuration
power_dt <- files_all[, list(power = (sum(reject_null)/.N)*100),
                      by=c('param_id','L','signal_noise_ratio')]

# -----------------------------------------------------------------------------
# For each signal-to-noise ratio, identify the smallest L where power exceeds 80%
# -----------------------------------------------------------------------------

# Order by signal_noise_ratio and L to ensure we find the *smallest* L
setorder(power_dt, signal_noise_ratio, L)

# Find L* for each signal-to-noise ratio
L_star_dt <- power_dt[power >= 80, .SD[1], by = signal_noise_ratio]
setnames(L_star_dt, "L", "L_star")

# Print results
cat("\n=== L* (smallest L with power >= 80%) by signal-to-noise ratio ===\n")
print(L_star_dt[, .(signal_noise_ratio, L_star, power)])

# Handle cases where power never exceeds 80%
snr_no_power <- setdiff(unique(power_dt$signal_noise_ratio), L_star_dt$signal_noise_ratio)
if (length(snr_no_power) > 0) {
  cat("\nWarning: The following signal-to-noise ratios never achieved 80% power:\n")
  print(snr_no_power)
  cat("Consider running with larger L values for these.\n")
}

# -----------------------------------------------------------------------------
# For each signal-to-noise ratio, create a new YAML scanning L by 1s
# -----------------------------------------------------------------------------

# Read the base config
base_config <- yaml::read_yaml(file.path(root, 'config_sim.yaml'))

# Output directory for new configs
config_out_dir <- file.path(root, 'refined_configs')
if (!dir.exists(config_out_dir)) {
  dir.create(config_out_dir, recursive = TRUE)
}

# Generate a YAML file for each signal-to-noise ratio
for (i in seq_len(nrow(L_star_dt))) {
  snr <- L_star_dt[i, signal_noise_ratio]
  l_star <- L_star_dt[i, L_star]

  # Create L values from L*-10 to L* (by 1s)
  # Ensure we don't go below 1
  l_min <- max(1, l_star - 10)
  l_values <- seq(l_min, l_star, by = 1)

  # Create new config
  new_config <- base_config
  new_config$signal_noise_ratio <- snr
  new_config$L <- as.list(l_values)

  # Write YAML file
  # Format SNR for filename (replace decimal point with underscore)
  snr_str <- gsub("\\.", "_", as.character(snr))
  yaml_filename <- file.path(config_out_dir, paste0("config_sim_snr_", snr_str, ".yaml"))

  yaml::write_yaml(new_config, yaml_filename)

  cat(sprintf("\nGenerated config for SNR=%.2f: %s\n", snr, yaml_filename))
  cat(sprintf("  L values: %s\n", paste(l_values, collapse = ", ")))
}

cat("\n=== Summary ===\n")
cat(sprintf("Generated %d YAML config files in: %s\n", nrow(L_star_dt), config_out_dir))
cat("\nTo run refined simulations, use these config files with the launcher.\n")
