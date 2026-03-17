# summarize run time

# args
version_id <- '20251019.01'

# directory
root <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id)
dir <- file.path(root,'batched_output')

# parameters
params <- fread(file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id, 'params.csv'))

# define function for processing each file
load_file <- function(file){
  dt <- fread(file)[measure=='total']
  dt[, param_id := as.numeric(str_extract(file, "(?<=_p)\\d+"))]
  dt[, batch_id := as.numeric(str_extract(file, "(?<=_b)\\d+"))]
  dt <- dt[,.(param_id, batch_id, batch_time, sim_time, fit_time, adj_time)]
} 

# combine observations
time_files <- list.files(dir, pattern="run_times_p", full.names = T)
time_stamps_all <- rbindlist(lapply(time_files, load_file))

# fix units
time_stamps_all[batch_time<3, batch_time := batch_time*60]

# summarize by param_id
time_stamps_summary <- time_stamps_all[, lapply(.SD, mean), by = param_id,
                                       .SDcols = c('batch_time','sim_time', 'fit_time', 'adj_time')]
time_stamps_summary <- merge(params[,.(param_id, B,R,L,t)], time_stamps_summary, by='param_id')


# save
fwrite(time_stamps_summary, paste0(root,'/run_time_summary.csv'))

# quick plots
pdf(paste0(root,'/run_time_investigation.pdf'), width=8, height=5)
ggplot(time_stamps_summary, aes(x=R, y=batch_time, color=as.character(L))) +
  geom_line() +
  geom_point() +
  facet_wrap(~t) +
  ggtitle('Batch Run Time') +
  scale_x_continuous("Number of reps per batch") +
  scale_y_continuous(name="Avg batch run time (mins)") +
  scale_color_discrete(name="Number of locs")

ggplot(time_stamps_summary, aes(x=R, y=fit_time/60, color=as.character(L))) +
  geom_line() +
  geom_point() +
  facet_wrap(~t) +
  ggtitle('Model Fit Run Time') +
  scale_x_continuous("Number of reps per batch") +
  scale_y_continuous(name="Avg time spent fitting the model (mins per batch)") +
  scale_color_discrete(name="Number of locs")

ggplot(time_stamps_summary, aes(x=R, y=adj_time/60, color=as.character(L))) +
  geom_line() +
  geom_point() +
  facet_wrap(~t) +
  ggtitle('UI Adjustment Run Time') +
  scale_x_continuous("Number of reps per batch") +
  scale_y_continuous(name="Avg time spent adjusting the UI (mins per batch)") +
  scale_color_discrete(name="Number of locs")
dev.off()

