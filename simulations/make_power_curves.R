# Make power curve

# args
version_id <- '20251031.01'

# directory
root <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id)
dir <- file.path(root,'batched_output')

# parameters
params <- fread(file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id, 'params.csv'))

# define function for processing each file
load_file <- function(file){
  dt <- fread(file)
  dt <- dt[, list(p_val = mean(p)), by=rep_id] #could add time_id==0 in case all time steps are output
  dt[, reject_null := ifelse(p_val<0.05,1,0)]
  dt[, param_id := as.numeric(str_extract(file, "(?<=_p)\\d+"))]
  dt[, batch_id := as.numeric(str_extract(file, "(?<=_b)\\d+"))]
  dt <- dt[,.(param_id, batch_id, rep_id, p_val, reject_null)]
} 

# combine observations and summarize by # of locations
list_of_files <- list.files(dir, pattern="pred_adj", full.names = T)
files_all <- rbindlist(lapply(list_of_files, load_file))
files_all <- merge(files_all, params[,.(param_id, L)], by='param_id')
power_dt <- files_all[, list(power = (sum(reject_null)/.N)*100), by=L]


# for a given # of locations, calculate power by rep
L40_files <- list.files(dir, pattern="pred_adj_p40", full.names = T)
L40_dt <- rbindlist(lapply(L40_files, load_file))
L40_dt [, power := cumsum(reject_null)/seq_along(reject_null)]
L40_dt[, rep := 1:nrow(L40_dt)]

L50_files <- list.files(dir, pattern="pred_adj_p41", full.names = T)
L50_dt <- rbindlist(lapply(L50_files, load_file))
L50_dt [, power := cumsum(reject_null)/seq_along(reject_null)]
L50_dt[, rep := 1:nrow(L50_dt)]



# plot
pdf(paste0(root,'/power_curve.pdf'), width=7,height=5)
ggplot(power_dt, aes(x=L, y=power)) +
  geom_line() +
  geom_hline(yintercept=80, linetype='dashed') +
  scale_x_continuous(name='Number of locations') +
  scale_y_continuous(labels=scales::percent_format(scale=1)) +
  ggtitle('How many locations required to reach 80% power given the following parameters? \ntheta=2,sigma=0.5,p=0.5,T=8, R=1000') +
  theme_bw() +
  theme(plot.title = element_text(size = 10))

ggplot(power_dt[L>=40], aes(x=L, y=power)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept=80, linetype='dashed') +
  scale_x_continuous(name='Number of locations') +
  scale_y_continuous(labels=scales::percent_format(scale=1)) +
  ggtitle('Zooming in \ntheta=2,sigma=0.5,p=0.5,T=8, R=1000') +
  theme_bw()

ggplot(L40_dt, aes(x=rep,y=power)) +
  geom_line() +
  geom_hline(yintercept=.8, linetype='dashed') +
  scale_x_continuous(name='Number of reps') +
  scale_y_continuous(labels=scales::percent) +
  ggtitle('How many reps until power stabilizes? \nL=40') +
  theme_bw()

ggplot(L50_dt, aes(x=rep,y=power)) +
  geom_line() +
  geom_hline(yintercept=.8, linetype='dashed') +
  scale_x_continuous(name='Number of reps') +
  scale_y_continuous(labels=scales::percent) +
  ggtitle('How many reps until power stabilizes? \nL=50') +
  theme_bw()

dev.off()
