# Pipeline by batch
# Launch the pipeline once for each rep in the batch

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
root_dir <- inputs$root_dir
out_dir <- inputs$out_dir
config_path <- file.path(root_dir,'inputs/config_files/config_test.yaml')
data_req_path <- file.path(root_dir,'inputs/config_files/min_data_requirement_by_model.csv')
param_path <- file.path(out_dir,'params.csv')
  
# Load all candidate models
list_of_files <- list.files(paste0(root_dir,"/code/models"), full.names = T)
for (file in list_of_files){
  source(file)
}
  
# Source the pipeline function
source(file.path(root_dir, "code/simulations/refactor/pipeline.R"))

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
                        problem_log = problem_log, root_dir = root_dir)
  
  
# Run the pipeline for each rep in the batch and combine results
batch_results <- data.table()
for (r in 1:param_set$R){
  one_rep <- pipeline(param_set, pipeline_inputs)
  ids <- data.table(batch_id = batch_id,
                    rep_id = rep(r, nrow(one_rep)),
                    loc_id = rep(1:param_set$L, each=length(unique(one_rep$time_id))))
  one_rep <- cbind(ids, one_rep)
  batch_results <- rbind(batch_results, one_rep)
  
  #TODO - every 10 reps, write to a file (to keep track of progress while job is running)
}  

# SAVE TO DISK
fwrite(batch_results, paste0(out_dir,'/batched_output/results_config_',inputs$param_id,'_batch_', batch_id,'.csv'))
