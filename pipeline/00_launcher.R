# Launch the pipeline with real data

# Define root directory
code_dir <- "/ihme/homes/ems2285/repos/simulate-pipeline"
config_path <- file.path(code_dir,'config_files/config_test.yaml') ###TEST FILE FOR DEV###
input_dir <- "/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/"
out_root <- "/ihme/scratch/users/ems2285/thesis/outputs/outputs/"

# Load functions
source(file.path(code_dir, 'helper_functions/get_version.R'))
source(file.path(code_dir, 'helper_functions/set_up.R'))
source(file.path(code_dir, 'helper_functions/submit_jobs.R'))
library(yaml)


## 1. SETUP OUTPUT DIRECTORY ##
out_dir <- set_up(out_root)
#out_dir <- file.path(out_root,'20260411.01')


## 2. LOAD CONFIG FILE AND LOOKUP TABLE ##

# Config file
configs <- read_yaml(config_path)
write_yaml(configs, paste0(out_dir,'/inputs/config.yaml'))

# Context look-up table
context_lookup <- fread(paste0(code_dir,'/config_files/context_lookup_table_',configs$lookup_suffix,'.csv'))
n_contexts <- nrow(context_lookup)
fwrite(context_lookup, paste0(out_dir,'/inputs/context_lookup_table.csv'))

# Location look-up table
loc_lookup <- fread(paste0(code_dir,'/config_files/locs_by_context_',configs$lookup_suffix,'.csv'))[context_id %in% context_lookup$context_id]
fwrite(loc_lookup, paste0(out_dir,'/inputs/locs_by_context.csv'))


## 3. LAUNCH JOB ##
# Launch the pipeline once for each context id (array job with one task per context)
message("Launching ", n_contexts, " context(s): ", paste(context_lookup$context_id, collapse = ", "))
sbatch(jobname = 'its_pipeline',
       code = file.path(code_dir, "pipeline/0_pipeline_wrapper.R"),
       pass = paste0('--code_dir ', code_dir,
                     ' --input_dir ', input_dir,
                     ' --out_dir ', out_dir),
       mem = '6G', 
       fthreads = 4, 
       #max_run_time = "02:00:00",
       q = "all.q",
       arr_len = n_contexts,
       log=T,
       e = file.path(out_dir, 'logs/error_context_%a.txt'),
       o = file.path(out_dir, 'logs/out_context_%a.txt'),
       submit=T
)


## 4. QC Reminder
# After all jobs finish, verify that expected output files exist:

#   files <- list.files(file.path(out_dir, 'batched_output'), pattern = 'pred_adj')
#   if (length(files) < n_contexts) message('Some output files are missing')


