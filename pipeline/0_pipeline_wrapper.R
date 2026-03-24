# Pipeline wrapper

# Load packages
library(argparse)
library(data.table)
library(yaml)
library(boot)
library(MASS)

# Get arguments from parser
parser <- ArgumentParser()
parser$add_argument("--code_dir", default = "none", type = 'character')
parser$add_argument("--input_dir", default = "none", type = 'character')
parser$add_argument("--out_dir", default = "none", type = 'character')
parser$add_argument("--group", default = "none", type = 'numeric')
args <- parser$parse_args()
print(args)
list2env(args, environment()); rm(args)

# Define paths
config_path <- file.path(code_dir,'config_files/config.yaml')
data_req_path <- file.path(code_dir,'config_files/min_data_requirement_by_model.csv')

# Source the pipeline script
source(file.path(code_dir, "pipeline/0_pipeline.R"))

# Load all candidate models
list_of_files <- list.files(paste0(code_dir,"/models"), full.names = T)
for (file in list_of_files){
  source(file)
}

# Load location group definition
loc_list <- unlist(strsplit(fread(paste0(out_dir,'/inputs/location_groups.csv'))[group_id==group]$locations, split=','))

# Prep input files
# A. Config file
configs <- read_yaml(config_path)
write_yaml(configs, paste0(out_dir,'/inputs/config.yaml'))
configs$location_list <- loc_list


# B. Min and max train periods
w <- configs$w
data_req_dt <- fread(data_req_path)
models <- configs$models
min_train_t <- max(data_req_dt[weeks_ahead==configs$w, ..models])

pipeline_inputs <- list(configs = configs, min_train_t = min_train_t,
                        code_dir = code_dir, input_dir = input_dir, group_id = group)

# RUN THE PIPELINE
result <- pipeline(pipeline_inputs)

# Combine coverage output into a single table
coverage_dt <- data.table(coverage_pre = result$coverage_pre,
                          coverage_post = result$coverage_post,
                          multiplier = result$multiplier)

# SAVE TO DISK
suffix <- paste0('group_',group) 
fwrite(result$obs_dt, paste0(out_dir,'/batched_output/obs_',suffix,'.csv'))
fwrite(result$candidate_mod_output, paste0(out_dir,'/batched_output/candidate_mods_',suffix,'.csv'))
fwrite(result$pre_adj_output, paste0(out_dir,'/batched_output/pred_pre_',suffix,'.csv'))
fwrite(result$results_output, paste0(out_dir,'/batched_output/pred_adj_',suffix,'.csv'))
fwrite(result$weights_dt, paste0(out_dir,'/batched_output/ens_weights_',suffix,'.csv'))
fwrite(result$sigmas_dt, paste0(out_dir,'/batched_output/sigmas_',suffix,'.csv'))
fwrite(coverage_dt, paste0(out_dir,'/batched_output/coverage_',suffix,'.csv'))

