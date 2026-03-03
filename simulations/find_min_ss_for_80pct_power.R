# Find the smallest value of L that yields >= 80% power
# Approach: 
# 1. Load results and calculate power by L and signal-to-noise ratio
# 2. Find the smallest L where power exceeds 80% (L*)
# 3. If L was run in bins of 10, generate a YAML file to re-run 
#    for values of L=[L*-10, L*-9,...,L*]. Then repeat steps 1-2.
# 4. If L was run in "bins" of 1, this is your final answer

library(data.table)
library(stringr)
library(yaml)

# Which run version id do you want to examine?
version_id <- '20260224.01'
# Do you want to look at pre- or post-adjusted ensemble predictions? ('pre' or 'adj' are valid options)
pred_type <- 'adj' 
# What is the scaling factor between parameter SNR and empirical/realized SNR?
scalar <- 1.32

# Directory
root <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id)
dir <- file.path(root,'batched_output')

# Parameters
params <- fread(file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id, 'params.csv'))

### 1. Load results and calculate power by L and signal-to-noise ratio

# Define function for processing each prediction file
load_file <- function(file){
  # define ids
  param <- as.numeric(str_extract(file, "(?<=_p)\\d+"))
  batch <- as.numeric(str_extract(file, "(?<=_b)\\d+"))
  
  # load data
  dt <- fread(file)[time_id==0, .(rep_id, location_id, time_id, p_val)]
  
  # tally up number of stat sig locs per rep
  dt[, verdict := ifelse(p_val<0.05,1,0)]
  rep_dt <- dt[, list(k = sum(verdict)), by=rep_id]
  rep_dt[, param_id := param]
  rep_dt[, batch_id :=  batch]
  rep_dt <- rep_dt[,.(param_id, batch_id, rep_id, k)]
} 

# Combine reps and calculate p-value/verdict for each rep
list_of_files <- list.files(dir, pattern=paste0("pred_",pred_type), full.names = T)
files_all <- rbindlist(lapply(list_of_files, load_file))
files_all <- merge(files_all, params[,.(param_id, L, signal_noise_ratio)], by='param_id')
files_all[, p_val := pbinom(k,L,0.05,lower.tail=F)]
files_all[, reject_null := ifelse(p_val <= 0.05, 1, 0)]

# Calculate power for each parameter configuration
power_dt <- files_all[, list(power = (sum(reject_null)/.N)*100),
                      by=c('param_id','L','signal_noise_ratio')]

### 2. For each signal-to-noise ratio, identify the smallest L where power exceeds 80%
setorder(power_dt, signal_noise_ratio, L)
L_star_dt <- power_dt[power>=80, .SD[1], by=signal_noise_ratio]
setnames(L_star_dt, "L", "L_star")

# Note any ratios where 80% power isn't reached (100 locs not sufficient)
setdiff(unique(power_dt$signal_noise_ratio), L_star_dt$signal_noise_ratio)

### 3. For each signal-to-noise ratio, create a new YAML scanning L by 1s

# Load original config file
base_config <- read_yaml(file.path(root,'config_sim.yaml'))

# Create an output directory for new configs
config_out_dir <- file.path(root, 'refined_configs')
if (!dir.exists(config_out_dir)) {
  dir.create(config_out_dir, recursive = TRUE)
}

# Generate a YAML file for each signal-to-noise ratio
for (i in seq_len(nrow(L_star_dt))) {
  snr <- L_star_dt[i, signal_noise_ratio]
  l_star <- L_star_dt[i, L_star]
  
  # Create L values from L*-10 to L* (by 1s)
  l_min <- max(1, l_star - 10) #ensure L>=1
  l_values <- seq(l_min, l_star, by = 1)
  
  # Create new config
  new_config <- base_config
  new_config$signal_noise_ratio <- snr
  new_config$L <- as.list(l_values)
  
  # Write YAML file
  # Format SNR for filename (replace decimal point with underscore)
  snr_str <- gsub("\\.", "_", as.character(snr))
  yaml_filename <- file.path(config_out_dir, paste0("config_sim_snr_", snr_str, ".yaml"))
  
  write_yaml(new_config, yaml_filename)
}

# stop. use the above configs with launcher.r
# come back to this script and run lines 1-44
# then proceed from line 103

### 4. For each signal-to-noise ratio, identify the smallest L where power exceeds 80%
version_list <- c(paste0('20260224.0', 2:4), paste0('20260225.0', 1:3))

final_dt <- data.table()
for (v in version_list){
  v_root <- paste0('/ihme/scratch/users/ems2285/thesis/outputs/simulations/', v)
  v_dir <- paste0(v_root,'/batched_output')
  
  # load params
  params <- fread(file.path(v_root, 'params.csv'))
  
  # load and combine results
  list_of_files <- list.files(v_dir, pattern=paste0("pred_",pred_type), full.names = T)
  files_all <- rbindlist(lapply(list_of_files, load_file))
  
  # add parameter info
  files_all <- merge(files_all, params[,.(param_id, L, signal_noise_ratio)], by='param_id')
  
  # calc p-value by rep
  files_all[, p_val := pbinom(k,L,0.05,lower.tail=F)]
  files_all[, reject_null := ifelse(p_val <= 0.05, 1, 0)]
  
  # Calculate power for each parameter configuration
  power_dt <- files_all[, list(power = (sum(reject_null)/.N)*100),
                        by=c('param_id','L','signal_noise_ratio')]
  
  # identify smallest L where power exceeds 80%
  setorder(power_dt, signal_noise_ratio, L)
  L_dt <- power_dt[power>=80, .SD[1], by=signal_noise_ratio]
  final_subset <- L_dt[, .(signal_noise_ratio, L)]
  
  # append to the final table
  final_dt <- rbind(final_subset, final_dt)
}


### 5. Translate SNR to reflect the realized sd of the simulations
final_dt[, SNR_new := signal_noise_ratio / scalar]

### 6. Make the results plot
pdf(paste0(root,'/snr_vs_L.pdf'), width=6, height=4)
ggplot(final_dt, aes(x = SNR_new, y = L)) +
  geom_smooth(se=FALSE) +
  geom_point() +
  scale_x_continuous(name='Signal-to-noise ratio', limits=c(0.11,0.3)) +
  scale_y_continuous(name='Number of Locations', limits=c(0,80), n.breaks=10) +
  theme_bw() +
  ggtitle('Sample Size Required to Achieve 80% Power \nat Varying Signal-to-Noise Ratios') +
  theme(axis.text = element_text(size=12), axis.title=element_text(size=12),
        plot.title = element_text(size=11, hjust = 0.5)) 
dev.off()


# improving the figure
ggplot(final_dt, aes(y=L, x = SNR_new)) +
  geom_smooth(se=FALSE) +
  geom_point() +
  scale_x_continuous(name='Signal-to-Noise Ratio', limits=c(0.25,0.75), breaks=seq(0.25,0.75,0.1)) +
  scale_y_continuous(name='Number of Locations', limits=c(0,80), n.breaks=10) +
  theme_bw() +
  ggtitle('Sample Size Required to Achieve 80% Power \nat Varying Signal-to-Noise Ratios') +
  theme(axis.text = element_text(size=12), axis.title=element_text(size=12), 
        plot.title = element_text(size=11, hjust = 0.5))
