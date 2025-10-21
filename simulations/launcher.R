# Sim launcher

# Define root directory
code_dir <- "/ihme/homes/ems2285/repos/simulate-pipeline"
out_root <- "/ihme/scratch/users/ems2285/thesis/outputs/simulations/"
source(file.path(code_dir, 'helper_functions/get_version.R'))
source(file.path(code_dir, 'helper_functions/submit_jobs.R'))
source(file.path(code_dir, 'simulations/set_up.R'))
source(file.path(code_dir, 'simulations/prep_configs.R'))


## 1. SETUP OUTPUT DIRECTORY ##
out_dir <- set_up(out_root)

## 2. CONFIG FILE ##
params <- prep_configs(config_dir = file.path(code_dir, "config_files"), 
                       out_dir = out_dir)


## 3. LAUNCH JOBS ##
# Launch a batch of jobs for each parameter combination
for (pc in 1:nrow(params)){
#for (pc in params[R==100, which=T]){

  # Define number of batches
  B <- params[param_id==pc, B]
  
  # Define inputs
  input_list <- list(code_dir = code_dir, out_dir = out_dir, param_id = pc)
  input_path <- paste0(out_dir,'/inputs/inputs_',pc,'.yaml')
  write_yaml(input_list, input_path)
  
  #TODO - throttle jobs
  for (b in 1:B){
    # launch the jobs
    sbatch(jobname = paste0('param_',pc,'_batch_',b),
           code = file.path(code_dir, "simulations/pipeline_by_batch.R"),
           pass = paste0('--input_path ', input_path, ' --batch_id ', b),
           mem = '5G',
           fthreads = 1, #up the number of threads
           log=T,
           e = file.path(out_dir,'logs/error_%A-%a.txt'),
           o = file.path(out_dir,'logs/out_%A-%a.txt'),
           submit=T
    )
  }
  
}

