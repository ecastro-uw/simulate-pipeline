# Make power curve

# args
version_id <- '20260109.01' #'20251201.04'

# directory
root <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id)
dir <- file.path(root,'batched_output')

# parameters
params <- fread(file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id, 'params.csv'))

# define function for processing each file
load_file <- function(file){
  dt <- fread(file)[time_id==0, .(rep_id, location_id, time_id, p_val)]
  dt <- dt[, list(p_val = mean(p_val)), by=rep_id] 
  dt[, reject_null := ifelse(p_val<0.05,1,0)]
  dt[, param_id := as.numeric(str_extract(file, "(?<=_p)\\d+"))]
  dt[, batch_id := as.numeric(str_extract(file, "(?<=_b)\\d+"))]
  dt <- dt[,.(param_id, batch_id, rep_id, p_val, reject_null)]
} 

# combine observations and summarize by # of locations
list_of_files <- list.files(dir, pattern="pred_adj", full.names = T)
files_all <- rbindlist(lapply(list_of_files, load_file))
files_all <- merge(files_all, params[,.(param_id, L, p.s)], by='param_id')
power_dt <- files_all[, list(power = (sum(reject_null)/.N)*100), by=c('L','p.s')]
lng_dt <- power_dt[order(p.s, L)]
wide_dt <- dcast(lng_dt, p.s ~ L, value.var='power')

ggplot(power_dt, aes(x=L,y=power)) +
  geom_line() +
  theme_bw() +
  ggtitle('L vs power at varying values of p.s') +
  facet_wrap(~p.s)

# make the power curve
ggplot(power_dt, aes(x=L, y=power)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept=80, linetype='dashed') +
  scale_x_continuous(name='Number of locations') +
  scale_y_continuous(labels=scales::percent_format(scale=1)) +
  #ggtitle('How many locations required to reach 80% power given the following parameters? \ntheta=2,sigma=0.5,p=0.5,T=8, R=1000') +
  theme_bw() +
  theme(plot.title = element_text(size = 10))


# for a given # of locations, calculate power by rep
L10_files <- list.files(dir, pattern="pred_adj_p1", full.names = T)
L10_dt <- rbindlist(lapply(L10_files, load_file))
L10_dt[, power := cumsum(reject_null)/seq_along(reject_null)]
L10_dt[, rep := 1:nrow(L10_dt)]

ggplot(L10_dt, aes(x=rep,y=power)) +
  geom_line() +
  geom_hline(yintercept=.8, linetype='dashed') +
  scale_x_continuous(name='Number of reps') +
  scale_y_continuous(labels=scales::percent) +
  ggtitle('How many reps until power stabilizes? \nL=10') +
  theme_bw()




# create pdf
pdf(paste0(root,'/power_curve.pdf'), width=7,height=5)
ggplot(power_dt, aes(x=L, y=power)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept=80, linetype='dashed') +
  scale_x_continuous(name='Number of locations') +
  scale_y_continuous(labels=scales::percent_format(scale=1)) +
  #ggtitle('How many locations required to reach 80% power given the following parameters? \ntheta=2,sigma=0.5,p=0.5,T=8, R=1000') +
  theme_bw() +
  theme(plot.title = element_text(size = 10))

#ggplot(power_dt[L>=40], aes(x=L, y=power)) +
#  geom_line() +
#  geom_point() +
#  geom_hline(yintercept=80, linetype='dashed') +
#  scale_x_continuous(name='Number of locations') +
#  scale_y_continuous(labels=scales::percent_format(scale=1)) +
#  ggtitle('Zooming in \ntheta=2,sigma=0.5,p=0.5,T=8, R=1000') +
#  theme_bw()

#ggplot(L40_dt, aes(x=rep,y=power)) +
#  geom_line() +
#  geom_hline(yintercept=.8, linetype='dashed') +
#  scale_x_continuous(name='Number of reps') +
#  scale_y_continuous(labels=scales::percent) +
#  ggtitle('How many reps until power stabilizes? \nL=40') +
#  theme_bw()

#ggplot(L50_dt, aes(x=rep,y=power)) +
#  geom_line() +
#  geom_hline(yintercept=.8, linetype='dashed') +
#  scale_x_continuous(name='Number of reps') +
#  scale_y_continuous(labels=scales::percent) +
#  ggtitle('How many reps until power stabilizes? \nL=50') +
#  theme_bw()

dev.off()


# investigating why power is 100% for L=10
# for time-step 0, p-value is almost always 0 across all locations and reps

# first, check the unadjusted p-value
# RESULT: p-value of unadjusted preds at time 0 is also 0 for all reps & locs
pre_dt <- fread(file.path(dir,'pred_pre_p1_b1.csv'))
obs_dt <- fread(file.path(dir,'obs_p1_b1.csv'))
pre_dt <- merge(obs_dt, pre_dt, by=c('rep_id', 'location_id', 'time_id'))

n_draws <- unique(params$d) 
draw_cols <- paste0('draw_',1:n_draws)
pre_dt$p_val <- rowSums(pre_dt[, lapply(.SD, function(x) x <= y), .SDcols = draw_cols])/n_draws

summary_dt <- pre_dt[time_id==0, list(p_val = mean(p_val)), by=rep_id]
