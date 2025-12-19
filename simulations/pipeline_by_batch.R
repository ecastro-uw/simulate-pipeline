# Pipeline by batch
# Launch the pipeline once for each rep in the batch
start_time <- Sys.time()

# Load packages
library(argparse)
library(data.table)
library(yaml)
library(foreach)
library(doParallel)
library(doRNG)

# Get arguments from parser
parser <- ArgumentParser()
parser$add_argument("--input_path", default = "none", type = 'character')
parser$add_argument("--batch_id", default = "none", type = 'numeric')
args <- parser$parse_args()
print(args)
list2env(args, environment()); rm(args)

# Load input file
inputs <- read_yaml(input_path)

# Define paths
code_dir <- inputs$code_dir
out_dir <- inputs$out_dir
config_path <- file.path(code_dir,'config_files/config.yaml')
data_req_path <- file.path(code_dir,'config_files/min_data_requirement_by_model.csv')
param_path <- file.path(out_dir,'params.csv')
  
# Load all candidate models
list_of_files <- list.files(paste0(code_dir,"/models"), full.names = T)
for (file in list_of_files){
  source(file)
}
  
# Source the pipeline function
source(file.path(code_dir, "simulations/pipeline.R"))

# Load parameter information
param_set <- fread(param_path)[param_id == inputs$param_id]

# Prep parameter-specific files
  
# A. Config file
configs <- read_yaml(config_path)
configs$d <- param_set$d
if(param_set$fit_model=='ensemble'){
  configs$models <- c('naive_flat_1', 'naive_slope_2')
} else {
  configs$models <- param_set$fit_model
}
  
# B. Min and max train periods
w <- configs$w
data_req_dt <- fread(data_req_path)
models <- configs$models
min_train_t <- max(data_req_dt[weeks_ahead==configs$w, ..models])
max_train_t <- param_set$t
  
# C. Problem log
problem_log <- data.table('location_id'=numeric(), 'instance'=numeric())
  
pipeline_inputs <- list(configs = configs, min_train_t = min_train_t, max_train_t = max_train_t,
                        problem_log = problem_log, code_dir = code_dir)
  
  
# Run the pipeline for each rep in the batch and combine results

# Set up parallel backend (4 cores)
cl <- makeCluster(4)
registerDoParallel(cl)

# Run parallel loop
results <- foreach(
  r = 1:param_set$R,
  .combine = function(x, y) {
    list(
      batch_obs = rbind(x$batch_obs, y$batch_obs),
      batch_pred_pre = rbind(x$batch_pred_pre, y$batch_pred_pre),
      batch_cov_pre = c(x$batch_cov_pre, y$batch_cov_pre),
      batch_cov_post = c(x$batch_cov_post, y$batch_cov_post),
      batch_mult = c(x$batch_mult, y$batch_mult),
      batch_pred_adj = rbind(x$batch_pred_adj, y$batch_pred_adj),
      batch_weights = rbind(x$batch_weights, y$batch_weights),
      batch_sigmas = rbind(x$batch_sigmas, y$batch_sigmas),
      batch_run_times = rbind(x$batch_run_times, y$batch_run_times)
    )
  },
  .packages = c("data.table", "boot")
  #.export = gsub(".R","", gsub(".*/models/", "", list_of_files))
) %dorng% {
  
  # Source custom functions on this worker
  list_of_files <- list.files(paste0(pipeline_inputs$code_dir, "/models"), full.names = TRUE)
  for (file in list_of_files) {
    source(file)
  }

  # Run pipeline for this rep
  one_rep <- pipeline(param_set, pipeline_inputs)
  
  # (1) Observations
  obs_dt <- one_rep$obs_dt
  obs_dt <- cbind(rep_id = r, obs_dt)
  
  # (2) Pre-adjustment forecasts
  pre_adj_output <- cbind(rep_id = r, one_rep$pre_adj_output)
  
  # (3) Pre-adjustment coverage rate
  cov_pre <- one_rep$coverage_pre
  
  # (4) Post-adjustment coverage rate
  cov_post <- one_rep$coverage_post
  
  # (5) Multiplier
  mult <- one_rep$multiplier
  
  # (6) Adjusted forecasts
  results_output <- cbind(rep_id = r, one_rep$results_output)
  
  # (7) Ensemble Weights
  weights_dt <- cbind(rep_id = r, one_rep$weights_dt)
  
  # (8) Sigmas
  sigmas_dt <- cbind(rep_id = r, one_rep$sigmas_dt)
  
  # (9) Time stamps
  time_stamps <- data.table(
    rep_id = r,
    sim_time = one_rep$time_stamps['sim_time'],
    pred_time = one_rep$time_stamps['pred_time'],
    ensemble_time = one_rep$time_stamps['ensemble_time'],
    adjust_time = one_rep$time_stamps['adjust_time']
  )
  
  # Return all results as a list for this iteration
  list(
    batch_obs = obs_dt,
    batch_pred_pre = pre_adj_output,
    batch_cov_pre = cov_pre,
    batch_cov_post = cov_post,
    batch_mult = mult,
    batch_pred_adj = results_output,
    batch_weights = weights_dt,
    batch_sigmas = sigmas_dt,
    batch_run_times = time_stamps
  )
} #END LOOP

# Stop the cluster when done
stopCluster(cl)

# Extract results
batch_obs <- results$batch_obs
batch_pred_pre <- results$batch_pred_pre
batch_cov_pre <- results$batch_cov_pre
batch_cov_post <- results$batch_cov_post
batch_mult <- results$batch_mult
batch_pred_adj <- results$batch_pred_adj
batch_weights <- results$batch_weights
batch_sigmas <- results$batch_sigmas
batch_run_times <- results$batch_run_times

# Combine vectorized output into a single table
coverage_dt <- data.table(rep_id = 1:length(results$batch_cov_pre),
                          coverage_pre = results$batch_cov_pre,
                          coverage_post = results$batch_cov_post,
                          multiplier = results$batch_mult)

# SAVE TO DISK
suffix <- paste0('p',inputs$param_id,'_b',batch_id)

fwrite(results$batch_obs, paste0(out_dir,'/batched_output/obs_',suffix,'.csv'))
fwrite(results$batch_pred_pre, paste0(out_dir,'/batched_output/pred_pre_',suffix,'.csv'))
fwrite(results$batch_pred_adj, paste0(out_dir,'/batched_output/pred_adj_',suffix,'.csv'))
fwrite(results$batch_weights, paste0(out_dir,'/batched_output/ens_weights_',suffix,'.csv'))
fwrite(results$batch_sigmas, paste0(out_dir,'/batched_output/sigmas_',suffix,'.csv'))
fwrite(coverage_dt, paste0(out_dir,'/batched_output/coverage_',suffix,'.csv'))

# Time stamps
end_time <- Sys.time()
batch_run_time <- end_time - start_time
# output all time stamps
batch_run_times <- results$batch_run_times
avg <- batch_run_times[, lapply(.SD, mean), .SDcols = c('sim_time', 'pred_time', 'ensemble_time', 'adjust_time')]
totals <- batch_run_times[, lapply(.SD, sum), .SDcols = c('sim_time', 'pred_time', 'ensemble_time', 'adjust_time')]
run_time_out <- data.table(measure = c('avg', 'total'),
           sim_time = c(avg$sim_time,totals$sim_time),
           pred_time = c(avg$pred_time,totals$pred_time),
           ensemble_time = c(avg$ensemble_time,totals$ensemble_time),
           adj_time = c(avg$adjust_time,totals$adjust_time),
           batch_time = c(NA, batch_run_time))
fwrite(run_time_out, paste0(out_dir,'/batched_output/run_times_',suffix,'.csv'))

print(batch_run_time)
