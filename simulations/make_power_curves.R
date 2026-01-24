# Make power curve


# Which run version id do you want to examine?
version_id <- '20260113.03' #'20251201.04'
# Do you want to look at pre- or post-adjusted ensemble predictions? ('pre' or 'adj' are valid options)
pred_type <- 'adj' 

# directory
root <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id)
dir <- file.path(root,'batched_output')

# parameters
params <- fread(file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id, 'params.csv'))

# define function for processing each file
load_file <- function(file){
  param <- as.numeric(str_extract(file, "(?<=_p)\\d+"))
  batch <- as.numeric(str_extract(file, "(?<=_b)\\d+"))
  n_draws <- params[param_id==param, d]
  if(grepl("pred_adj", file)){
    # post-adjusted results
    dt <- fread(file)[time_id==0, .(rep_id, location_id, time_id, p_val)]
  } else {
    # pre-adjusted results
    
    # load obs
    obs_dt <- fread(paste0(dir,'/obs_p', param, '_b', batch, '.csv'))
    # load preds
    dt <- fread(file)[time_id==0]
    # combine
    dt <- merge(obs_dt, dt, by=c('rep_id', 'location_id', 'time_id'))
    # calc p-value
    draw_cols <- paste0('draw_',1:n_draws)
    dt$p_val <- rowSums(dt[, lapply(.SD, function(x) x <= y), .SDcols = draw_cols])/n_draws
    dt <- dt[, (draw_cols) := NULL]
    dt$y <- NULL
  }
  dt[, verdict := ifelse(p_val<0.05,1,0)]
  rep_dt <- dt[, list(k = sum(verdict)), by=rep_id]
  rep_dt[, param_id := param]
  rep_dt[, batch_id :=  batch]
  rep_dt <- rep_dt[,.(param_id, batch_id, rep_id, k)]
} 

# combine reps and calculate p-value/verdict for each rep
list_of_files <- list.files(dir, pattern=paste0("pred_",pred_type), full.names = T)
files_all <- rbindlist(lapply(list_of_files, load_file))
files_all <- merge(files_all, params[,.(param_id, L, p.s)], by='param_id')
files_all[, p_val := pbinom(k,L,0.05,lower.tail=F)]
files_all[, reject_null := ifelse(p_val <= 0.05, 1, 0)]

# calculate power for each parameter configuration
power_dt <- files_all[, list(power = (sum(reject_null)/.N)*100), by=c('param_id','p.s','L')]

lng_dt <- power_dt[order(p.s, L)]
wide_dt <- dcast(lng_dt, p.s ~ L, value.var='power')
fwrite(wide_dt, paste0(root,'/power_table_PRE.csv'))

p2 <- ggplot(power_dt, aes(x=L,y=power, color=as.factor(p.s))) +
  geom_line() +
  theme_bw() +
  #ggtitle("Adjusted") +
  ggtitle(paste0('theta=',unique(params$theta),
                 ', sigma.f=',unique(params$sigma.f),
                 ', sigma.s=', unique(params$sigma.s))) +
  scale_color_discrete(name='p.s') +
  scale_x_continuous(limits=c(10,60), n.breaks=6) +
  scale_y_continuous(limits = c(20,100), n.breaks=5) 

pdf(paste0(root,'/power_curve_POST.pdf'), width=7,height=5)
p2
dev.off()

# Combine plots into a grid
plot_list <- list(p1,p2)
combined_plot <- wrap_plots(plot_list, ncol = 2) + plot_layout(guides="collect")

# Add title
plot_b <- combined_plot + plot_annotation(
  title = paste0('theta=',unique(params$theta),
                 ', sigma.f=',unique(params$sigma.f),
                 ' & sigma.s=', unique(params$sigma.s)),
  theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
)

pdf(paste0(root,'/power_curve_BOTH.pdf'), width=11,height=5)
plot_b
dev.off()


# Once the power table has been saved, no need to re-summarize
pre_wide <- fread(paste0(root,'/power_table_PRE.csv'))
pre_dt <- melt(pre_wide, id.vars='theta', variable.name='L', value.name='power')
pre_dt[, L := as.numeric(as.character(L))]

post_wide <- fread(paste0(root,'/power_table_POST.csv'))
post_dt <- melt(post_wide, id.vars='theta', variable.name='L', value.name='power')
post_dt[, L := as.numeric(as.character(L))]

p.1 <- ggplot(pre_dt, aes(x=L,y=power, color=as.factor(theta))) +
  geom_line() +
  theme_bw() +
  ggtitle("Unadjusted") +
  scale_color_discrete(name='theta') +
  scale_x_continuous(limits=c(10,60), n.breaks=6) +
  scale_y_continuous(limits = c(20,100), n.breaks=5) 

p.2 <- ggplot(post_dt, aes(x=L,y=power, color=as.factor(theta))) +
  geom_line() +
  theme_bw() +
  ggtitle("Adjusted") +
  scale_color_discrete(name='theta') +
  scale_x_continuous(limits=c(10,60), n.breaks=6) +
  scale_y_continuous(limits = c(20,100), n.breaks=5) 


# Combine plots into a grid
plot_list <- list(p.1,p.2)
combined_plot <- wrap_plots(plot_list, ncol = 2) + plot_layout(guides="collect")

# Add title
plot_b <- combined_plot + plot_annotation(
  title = paste0('sigma.f=',unique(params$sigma.f), ' & sigma.s=', unique(params$sigma.s)),
  theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
)

pdf(paste0(root,'/power_curve_BOTH_v2.pdf'), width=11,height=5)
plot_b
dev.off()




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
