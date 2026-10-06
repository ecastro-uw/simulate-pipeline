
# directory
version_id <- '20260514.01'
root_dir <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/outputs',version_id)


# optim fit files
fit_files <- list.files(file.path(root_dir, 'batched_output'), pattern = 'ens_fit_stats', full.names=T)
dt_all <- rbindlist(lapply(fit_files, function(x) {
  dt <- fread(x)
  dt[, context_id := as.integer(gsub('.*_context_(\\d+)\\.csv$', '\\1', basename(x)))]
  dt
}))


# context lookup info
lookup_dt <- fread(paste0(root_dir,'/inputs/context_lookup_table.csv'))
col_names <- paste0('model_', 1:31)
models_dt <- lookup_dt[, n_models := rowSums(.SD), .SDcols = col_names]
models_dt <- models_dt[, .(context_id, mandate_type, mandate_num, pop_cat, pol_cat, N, n_models)]

# combine
converge_dt <- merge(models_dt, dt_all, by='context_id')

# save
fwrite(converge_dt, paste0(root_dir, '/convergence.csv'))
