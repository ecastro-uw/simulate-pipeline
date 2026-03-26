# Launch the pipeline with real data

# Define root directory
code_dir <- "/ihme/homes/ems2285/repos/simulate-pipeline"
input_dir <- "/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/"
out_root <- "/ihme/scratch/users/ems2285/thesis/outputs/outputs/"
source(file.path(code_dir, 'helper_functions/get_version.R'))
source(file.path(code_dir, 'helper_functions/set_up.R'))
source(file.path(code_dir, 'helper_functions/submit_jobs.R'))


## 1. SETUP OUTPUT DIRECTORY ##
out_dir <- set_up(out_root)
#out_dir <- file.path(out_root,'20260325.01')

## 2. LOAD LOCATION GROUPS ##
loc_groups <- fread('/ihme/scratch/users/ems2285/thesis/aim_3/location_groups.csv')
fwrite(loc_groups, paste0(out_dir,'/inputs/location_groups.csv'))

## 3. LAUNCH JOB ##
# Launch the pipeline once for each grouping of locations
for (gr in 1:nrow(loc_groups)){
  sbatch(jobname = paste0('its_pipeline_group_',gr),
         code = file.path(code_dir, "pipeline/0_pipeline_wrapper.R"),
         pass = paste0('--code_dir ', code_dir, ' --input_dir ', input_dir,
                       ' --out_dir ', out_dir, ' --group ', gr),
         mem = '6G', 
         fthreads = 4, 
         #max_run_time = "02:00:00",
         q = "all.q",
         log=T,
         e = file.path(out_dir, paste0('logs/error_group_', gr, '.txt')),
         o = file.path(out_dir, paste0('logs/out_group_', gr, '.txt')),
         submit=T
  )
}

## 3. QC
# Check that all expected files are present.
files <- list.files(path=file.path(out_dir, 'batched_output'), pattern = 'pred_adj')
num_files <- length(files)
num_expected <- nrow(loc_groups)

if(num_files < num_expected){
  print('Some files are missing')
}
