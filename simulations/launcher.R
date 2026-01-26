# Sim launcher

# Define root directory
code_dir <- "/ihme/homes/ems2285/repos/simulate-pipeline"
out_root <- "/ihme/scratch/users/ems2285/thesis/outputs/simulations/"
source(file.path(code_dir, 'helper_functions/get_version.R'))
source(file.path(code_dir, 'helper_functions/submit_jobs.R'))
source(file.path(code_dir, 'helper_functions/throttle_jobs.R'))
source(file.path(code_dir, 'simulations/set_up.R'))
source(file.path(code_dir, 'simulations/prep_configs.R'))

## THROTTLING SETTINGS ##
max_concurrent_jobs <- 100  # Maximum number of jobs to run at once
throttle_check_interval <- 30  # Seconds between checking job queue


## 1. SETUP OUTPUT DIRECTORY ##
out_dir <- set_up(out_root)
#out_dir <- file.path(out_root,'20260124.01')

## 2. CONFIG FILE ##
params <- prep_configs(config_dir = file.path(code_dir, "config_files"), 
                       out_dir = out_dir)
#params <- fread(file.path(out_dir,'params.csv'))


## 3. LAUNCH JOBS ##
# Launch a batch of jobs for each parameter combination
#for (pc in 1:nrow(params)){
for (pc in 6:nrow(params)){
  
  # Define number of batches
  B <- params[param_id==pc, B]
  
  # Define inputs
  input_list <- list(code_dir = code_dir, out_dir = out_dir, param_id = pc)
  input_path <- paste0(out_dir,'/inputs/inputs_',pc,'.yaml')
  write_yaml(input_list, input_path)
  
  for (b in 1:B){
    # Wait for a slot if we're at the job limit
    wait_for_slot(max_jobs = max_concurrent_jobs,
                  check_interval = throttle_check_interval)

    # launch the jobs
    sbatch(jobname = paste0('param_',pc,'_batch_',b),
           code = file.path(code_dir, "simulations/pipeline_by_batch.R"),
           pass = paste0('--input_path ', input_path, ' --batch_id ', b),
           mem = '6G', #if running large L, may need to increase but 6G is usually sufficient
           fthreads = 8, #8 or 16 or 32 - fewer big jobs or more small jobs?
           #max_run_time
           log=T,
           e = file.path(out_dir,'logs/error_%A-%a.txt'),
           o = file.path(out_dir,'logs/out_%A-%a.txt'),
           submit=T
    )
  }
  
}

