# Pipeline wrapper
# Intended to run as a SLURM array job task
# SLURM_ARRAY_TASK_ID = context_id

# Load packages
library(argparse)
library(data.table)
library(yaml)
library(boot)
library(MASS)
library(forecast)


# Get arguments from parser
parser <- ArgumentParser()
parser$add_argument("--code_dir",  default = "none", type = 'character')
parser$add_argument("--input_dir", default = "none", type = 'character')
parser$add_argument("--out_dir",   default = "none", type = 'character')
args <- parser$parse_args()
print(args)
list2env(args, environment()); rm(args)

# Resolve context_id from the SLURM array task id
context <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", unset=1))

# Define paths
config_path <- file.path(out_dir,'inputs/config.yaml')
context_lookup_path <- file.path(out_dir,'inputs/context_lookup_table.csv')
loc_lookup_path <- file.path(out_dir,'inputs/locs_by_context.csv')

# Source the pipeline script
source(file.path(code_dir, "pipeline/0_pipeline.R"))

# Load look-up tables saved by the launcher
context_info <- fread(context_lookup_path)[context_id==context]
loc_list <- fread(loc_lookup_path)[context_id==context]$location_id
if (length(loc_list)==0) {
  stop("No locations found in locs_by_context.csv for context_id: ", context)
}

# Load all candidate models associated with the context id
cols <- grep("^model_", names(context_info), value = TRUE)
list_of_models <- cols[unlist(context_info[1, ..cols]) == 1]
list_of_files <- paste0(code_dir,"/models/",list_of_models,".R")
for (file in list_of_files){
  source(file)
}

# Prep pipeline inputs
configs <- read_yaml(config_path)
configs$country <- context_info$country
configs$location_type <- ifelse(context_info$ADMN==1,'states','counties')
configs$mandates <- context_info$mandate_type
configs$imposition <- context_info$mandate_num
configs$data_source <- ifelse(context_info$outcome=='retail visits', 'google', 'safegraph')
configs$models <- list_of_models

pipeline_inputs <- list(configs = configs,
                        code_dir = code_dir,
                        input_dir = input_dir,
                        out_dir = out_dir,
                        context_id = context,
                        loc_list = loc_list)


# RUN THE PIPELINE
result <- pipeline(pipeline_inputs)

# Combine coverage output into a single table
coverage_dt <- data.table(
  coverage_pre = result$coverage_pre,
  coverage_post = result$coverage_post,
  multiplier = result$multiplier,
  multiplier2 = result$multiplier2
)

# SAVE TO DISK
suffix <- paste0('context_',context) 
fwrite(result$obs_dt,               paste0(out_dir,'/batched_output/obs_',           suffix,'.csv'))
fwrite(result$candidate_mod_output, paste0(out_dir,'/batched_output/candidate_mods_',suffix,'.csv'))
fwrite(result$pre_adj_output,       paste0(out_dir,'/batched_output/pred_pre_',      suffix,'.csv'))
fwrite(result$results_output,       paste0(out_dir,'/batched_output/pred_adj_',      suffix,'.csv'))
fwrite(result$results_output2,      paste0(out_dir,'/batched_output/pred_adj2_',     suffix,'.csv'))
fwrite(result$weights_dt,           paste0(out_dir,'/batched_output/ens_weights_',    suffix,'.csv'))
fwrite(result$fit_stats_dt,         paste0(out_dir,'/batched_output/ens_fit_stats_',  suffix,'.csv'))
fwrite(result$sigmas_dt,            paste0(out_dir,'/batched_output/sigmas_',         suffix,'.csv'))
fwrite(coverage_dt,                 paste0(out_dir,'/batched_output/coverage_',      suffix,'.csv'))

