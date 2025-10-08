#pipeline by batch

pipeline_by_batch <- function(param_config, out_dir){
  
  jobs_dt <- fread(file.path(out_dir,'jobs.csv'))[param_id==param_config]
  
  # Launch an array job with b jobs
  # Each job runs the pipeline B times
  array_length <- nrow(jobs_dt)
  array_throttle <- 300
  if (array_length > array_throttle){
    array_jobs <- paste0(array_length,'%',array_throttle)
  } else {
    array_jobs <- array_length
  }
  
  # 
  log_dir <- file.path(out_dir,'logs')
  pass_vars <- paste0('--root_dir ', root_dir, ' --out_dir ', out_dir,
                      ' --param_config ', param_config)
  
  # 
  sbatch(jobname = paste0('array_job_',run_id),
         code = file.path(root_dir, "code/simulations/refactor/rep_wrapper.R"),
         pass = pass_vars,
         mem = '5G',
         fthreads = 1,
         log=T,
         arr_len = array_jobs,
         e = file.path(log_dir,'error_%A-%a.txt'),
         o = file.path(log_dir,'out_%A-%a.txt'),
         submit=T
  )
  
}