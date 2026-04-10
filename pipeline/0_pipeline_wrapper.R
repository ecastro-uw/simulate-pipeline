# Pipeline wrapper
# Intended to run as a SLURM array job task.
# SLURM_ARRAY_TASK_ID (1-indexed) selects the context from out_dir/inputs/contexts.txt.

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

# Resolve context_id from the SLURM array task index
task_id          <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", unset = "1"))
contexts         <- readLines(file.path(out_dir, 'inputs/contexts.txt'))
this_context_id  <- as.integer(contexts[task_id])
message("Task ", task_id, " -> context_id: ", this_context_id)

# Load lookup tables saved by the launcher
locs_by_context <- fread(file.path(out_dir, 'inputs/locs_by_context.csv'))
context_lookup  <- fread(file.path(out_dir, 'inputs/context_lookup_table.csv'))

# Gather all location IDs for this context
# locs_by_context.csv has one row per location_id (columns: context_id, location_id)
loc_list <- locs_by_context[context_id == this_context_id, location_id]
if (length(loc_list) == 0) {
  stop("No locations found in locs_by_context.csv for context_id: ", this_context_id)
}

# Get context-specific settings row
ctx_settings <- context_lookup[context_id == this_context_id]

# Define paths
config_path   <- file.path(code_dir, 'config_files/config.yaml')
data_req_path <- file.path(code_dir, 'config_files/min_data_requirement_by_model.csv')

# Source the pipeline script
source(file.path(code_dir, "pipeline/0_pipeline.R"))

# Load all candidate models
list_of_files <- list.files(paste0(code_dir, "/models"), full.names = TRUE)
for (file in list_of_files) source(file)

# Load base config and apply context-specific overrides
configs <- read_yaml(config_path)

configs$location_list  <- loc_list
configs$mandates   <- ctx_settings$mandate_type                              # "restaurant" or "bar"
configs$imposition <- c("1" = "first", "2" = "second")[as.character(ctx_settings$mandate_num)]

# Override country / location_type if present in the lookup table
if ('country' %in% names(ctx_settings))       configs$country       <- ctx_settings$country
if ('location_type' %in% names(ctx_settings)) configs$location_type <- ctx_settings$location_type

# Build model list from binary flags (model_1 ... model_20).
# NOTE: once model_1.R ... model_20.R are added, update
#       config_files/min_data_requirement_by_model.csv to use the same column names.
model_cols     <- grep("^model_", names(ctx_settings), value = TRUE)
active_models  <- model_cols[as.integer(ctx_settings[, ..model_cols]) == 1]
if (length(active_models) == 0) {
  stop("No models flagged as active (value=1) in context_lookup_table.csv for context_id: ",
       this_context_id)
}
configs$models <- active_models

# Save per-context config snapshot for reproducibility
write_yaml(configs, file.path(out_dir, 'inputs', paste0('config_', this_context_id, '.yaml')))

# Compute minimum training period for this context's model set
w           <- configs$w
data_req_dt <- fread(data_req_path)
models      <- configs$models
min_train_t <- max(data_req_dt[weeks_ahead == w, ..models])

pipeline_inputs <- list(
  configs     = configs,
  min_train_t = min_train_t,
  code_dir    = code_dir,
  input_dir   = input_dir,
  out_dir     = out_dir,
  group_id    = this_context_id   # used by prep_data() for logging dropped locations
)

# RUN THE PIPELINE
result <- pipeline(pipeline_inputs)

# Combine coverage output into a single table
coverage_dt <- data.table(
  coverage_pre  = result$coverage_pre,
  coverage_post = result$coverage_post,
  multiplier    = result$multiplier
)

# SAVE TO DISK
suffix <- paste0('context_', this_context_id)
fwrite(result$obs_dt,               file.path(out_dir, 'batched_output', paste0('obs_',            suffix, '.csv')))
fwrite(result$candidate_mod_output, file.path(out_dir, 'batched_output', paste0('candidate_mods_', suffix, '.csv')))
fwrite(result$pre_adj_output,       file.path(out_dir, 'batched_output', paste0('pred_pre_',       suffix, '.csv')))
fwrite(result$results_output,       file.path(out_dir, 'batched_output', paste0('pred_adj_',       suffix, '.csv')))
fwrite(result$weights_dt,           file.path(out_dir, 'batched_output', paste0('ens_weights_',    suffix, '.csv')))
fwrite(result$sigmas_dt,            file.path(out_dir, 'batched_output', paste0('sigmas_',         suffix, '.csv')))
fwrite(coverage_dt,                 file.path(out_dir, 'batched_output', paste0('coverage_',       suffix, '.csv')))
