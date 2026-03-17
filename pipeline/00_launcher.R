# Launch the pipeline with real data

# Define root directory
code_dir <- "/ihme/homes/ems2285/repos/simulate-pipeline"
out_root <- "/ihme/scratch/users/ems2285/thesis/outputs/"
source(file.path(code_dir, 'helper_functions/get_version.R'))
source(file.path(code_dir, 'helper_functions/set_up.R'))
source(file.path(code_dir, 'helper_functions/submit_jobs.R'))


## 1. SETUP OUTPUT DIRECTORY ##
out_dir <- set_up(out_root)

sbatch(jobname = paste0('param_',pc,'_batch_',b),
       code = file.path(code_dir, "pipeline/0_pipeline.R"),
       pass = paste0('--pipeline_inputs ', pipeline_inputs),
       mem = '6G', 
       fthreads = 4, 
       max_run_time = "02:00:00",
       q = "all.q",
       log=T,
       e = file.path(out_dir, paste0('logs/error_p', pc, '_b', b, '.txt')),
       o = file.path(out_dir, paste0('logs/out_p', pc, '_b', b, '.txt')),
       submit=T
)