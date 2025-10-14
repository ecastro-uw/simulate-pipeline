# Pipeline by batch
# Launch the pipeline once for each rep in the batch
start_time <- Sys.time()

# Load packages
library(argparse)
library(data.table)
library(yaml)

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
root_dir <- inputs$root_dir
out_dir <- inputs$out_dir
config_path <- file.path(root_dir,'inputs/config_files/config_test.yaml')
data_req_path <- file.path(root_dir,'inputs/config_files/min_data_requirement_by_model.csv')
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
                        problem_log = problem_log, root_dir = root_dir, code_dir = code_dir)
  
  
# Run the pipeline for each rep in the batch and combine results
batch_obs <- data.table()
batch_pred_pre <- data.table()
batch_cov_pre <- c()
batch_cov_post <- c()
batch_mult <- c()
batch_pred_adj <- data.table()
for (r in 1:param_set$R){
  one_rep <- pipeline(param_set, pipeline_inputs)
  
  # (1) Observations
  obs_dt <- one_rep$obs_dt
  obs_dt <- cbind(data.table(rep_id = rep(r, nrow(obs_dt))), obs_dt)
  batch_obs <- rbind(batch_obs, obs_dt)
  
  # (2) Pre-adjustment forecasts
  ids <- data.table(#batch_id = batch_id,
                    rep_id = rep(r, nrow(one_rep$pre_adj_output)),
                    location_id = rep(1:param_set$L, each=length(unique(one_rep$pre_adj_output$time_id))))
  pre_adj_output <- cbind(ids, one_rep$pre_adj_output)
  batch_pred_pre <- rbind(batch_pred_pre, pre_adj_output)
  
  # (3) Pre-adjustment coverage rate
  batch_cov_pre <- c(batch_cov_pre, one_rep$coverage_pre)
  
  # (4) post-adjustment coverage rate
  batch_cov_post <- c(batch_cov_post, one_rep$coverage_post)
  
  # (5) Multiplier
  batch_mult <- c(batch_mult, one_rep$multiplier)
  
  # (6) Adjusted forecasts
  ids <- data.table(#batch_id = batch_id,
                    rep_id = rep(r, nrow(one_rep$results_output)),
                    location_id = rep(1:param_set$L, each=length(unique(one_rep$results_output$time_id))))
  results_output <- cbind(ids, one_rep$results_output)
  batch_pred_adj <- rbind(batch_pred_adj, results_output)
  
  #TODO - every 10 reps, write to a file (to keep track of progress while job is running)
}  

coverage_dt <- data.table(rep_id = 1:length(batch_cov_pre),
                          coverage_pre = batch_cov_pre,
                          coverage_post = batch_cov_post,
                          multiplier = batch_mult)

# SAVE TO DISK
suffix <- paste0('p',inputs$param_id,'_b',batch_id)

fwrite(batch_obs, paste0(out_dir,'/batched_output/obs_',suffix,'.csv'))
fwrite(batch_pred_pre, paste0(out_dir,'/batched_output/pred_pre_',suffix,'.csv'))
fwrite(batch_pred_adj, paste0(out_dir,'/batched_output/pred_adj_',suffix,'.csv'))
fwrite(coverage_dt, paste0(out_dir,'/batched_output/coverage_',suffix,'.csv'))

end_time <- Sys.time()
elapsed_time <- end_time - start_time
print(elapsed_time)
