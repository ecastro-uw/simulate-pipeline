library(data.table)
library(stringr)

version_id <- '20251014.01'
param_id <- 1

dir <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id,'batched_output')

# params
params <- fread(file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id, 'params.csv'))
num_batches <- params$B

# load file and add batch id
load_file <- function(file){
  dt <- fread(file)
  dt[, batch_id := as.numeric(str_extract(file, "(?<=_b)\\d+"))]
  setcolorder(dt, c("batch_id", setdiff(names(dt), "batch_id")))
} 
              
# combine observations
obs_files <- list.files(dir, pattern=paste0("obs_p",param_id), full.names = T)
if(length(obs_files) == num_batches){
  obs_all <- rbindlist(lapply(obs_files, load_file))
}
obs_all <- obs_all[order(batch_id, rep_id, location_id, time_id)]

# combine pre-adjustment predictions
pred_pre_files <- list.files(dir, pattern=paste0("pred_pre_p",param_id), full.names = T)
if(length(pred_pre_files) == num_batches){
  pred_pre_all <- rbindlist(lapply(pred_pre_files, load_file))
}
pred_pre_all <- pred_pre_all[order(batch_id, rep_id, location_id, time_id)]
    
# combine adjusted predictions
pred_adj_files <- list.files(dir, pattern=paste0("pred_adj_p",param_id), full.names = T)
if(length(pred_adj_files) == num_batches){
  pred_adj_all <- rbindlist(lapply(pred_adj_files, load_file))
}  
pred_adj_all <- pred_adj_all[order(batch_id, rep_id, location_id, time_id)]

# combine coverage stats
coverage_files <- list.files(dir, pattern=paste0("coverage_p",param_id), full.names = T)
if(length(coverage_files) == num_batches){
  coverage_all <- rbindlist(lapply(coverage_files, load_file))
}
coverage_all <- coverage_all[order(batch_id, rep_id)]

# save?
fwrite(pred_adj_all, paste0('/ihme/scratch/users/ems2285/thesis/outputs/simulations/', version_id,
                            '/pred_adj_p', param_id,'.csv'))
fwrite(pred_pre_all, paste0('/ihme/scratch/users/ems2285/thesis/outputs/simulations/', version_id,
                            '/pred_pre_p', param_id,'.csv'))

       