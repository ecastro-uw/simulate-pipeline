# Calculate empirical SD

version_id <- '20260216.01'

# directory
root <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id)
dir <- file.path(root,'batched_output')

# parameters
params <- fread(file.path(root, 'params.csv'))

# For each observation file, calculate the change in y for the last 4 data points
load_one_file <- function(file){
  dt <- fread(file)[time_id>-5 & time_id<0]
  dt[, delta := y - shift(y, type="lag"), by=c('rep_id', 'location_id')]
  dt <- dt[! is.na(delta)]
  dt[, param_id := as.numeric(str_extract(file, "(?<=_p)\\d+"))]
  dt[, batch_id := as.numeric(str_extract(file, "(?<=_b)\\d+"))]
}
list_of_files <- list.files(dir, pattern=paste0("obs_"), full.names = T)
all_files <- rbindlist(lapply(list_of_files, load_one_file))

# calculate sd for each param_id
summary_dt <- all_files[, sd(delta), by=param_id]
setnames(summary_dt, 'V1', 'sigma.r')

# add parameter values
summary_dt <- merge(params[, .(param_id, L, sigma.f, sigma.s, theta, p.s)], summary_dt, by='param_id')

# make labels
summary_dt[, L_lab := factor(L, 
                             levels=unique(summary_dt$L),
                             labels=paste('L =', unique(summary_dt$L)))]
summary_dt[, p_lab := factor(p.s, 
                             levels=unique(summary_dt$p.s),
                             labels=paste('p_s =', unique(summary_dt$p.s)))]

# plot
pdf(file.path(root,'realized_sigma_v2.pdf'), width=7, height=5)
ggplot(summary_dt, aes(x=sigma.f,y=sigma.r)) +
  geom_smooth(method='lm', se=FALSE) +
  geom_point() +
  scale_x_continuous(name='sigma parameter', limits=c(0,10), breaks=seq(0,10,2)) + 
  scale_y_continuous(name='empirical sigma', limits=c(0,20), breaks=seq(0,20,4)) + 
  facet_grid(p_lab~L_lab) +
  theme_bw() +
  ggtitle('Relationship between noise parameters and empirical standard deviation \nDelta of last 3 pre-event timesteps (t=-3,-2,-1)')
dev.off()



### Try summarizing by group ###

# Calc the mean, sd, and sample size of each location-run
loc_rep_stats <- all_files[, .(n=.N, mu=mean(y), s=sd(y)), by=.(param_id, batch_id, rep_id, location_id)]
# Alternatively, calc mean, sd and SS of each simulation run
loc_rep_stats <- all_files[, .(n=.N, mu=mean(y), s=sd(y)), by=.(param_id, batch_id, rep_id)]


# Combine SDs across location-runs to get a single SD for each param_id
summary_dt_v2 <- data.table(param_id=1:nrow(params), sigma.r=as.numeric(''))
for (param in 1:nrow(params)){
  
  # subset to location-runs
  small_stats <- loc_rep_stats[param_id==param]
  
  # combine across reps
  N <- small_stats[, sum(n)]
  grand_mean <- small_stats[, sum(n * mu)] / N
  var_within <- small_stats[, sum(n * s^2)] / N
  var_between <- small_stats[, sum(n * (mu - grand_mean)^2)] / N
  sd_total <- sqrt(var_within + var_between)
  summary_dt_v2[param_id==param, sigma.r:= sd_total]
}

# Add parameter values
summary_dt_v2 <- merge(params[, .(param_id, L, sigma.f, sigma.s, theta, p.s)], summary_dt_v2, by='param_id')

# make labels
summary_dt_v2[, L_lab := factor(L, 
                             levels=unique(summary_dt_v2$L),
                             labels=paste('L =', unique(summary_dt_v2$L)))]
summary_dt_v2[, p_lab := factor(p.s, 
                             levels=unique(summary_dt_v2$p.s),
                             labels=paste('p_s =', unique(summary_dt_v2$p.s)))]

# plot
pdf(file.path(root,'realized_sigma_v2.pdf'), width=7, height=5)
ggplot(summary_dt_v2, aes(x=sigma.f,y=sigma.r)) +
  geom_smooth(method='lm', se=FALSE) +
  geom_point() +
  scale_x_continuous(name='sigma parameter', breaks=seq(1,10,2)) + 
  scale_y_continuous(name='empirical sigma', breaks=seq(0,50,10)) + 
  facet_grid(p_lab~L_lab) +
  theme_bw() +
  ggtitle('Relationship between noise parameters and empirical standard deviation')
dev.off()


# summarize the relationship between sigma and sigma.r when p.s=5
lm(sigma.r ~ sigma.f, data=summary_dt[p.s==0.75])


###
dt_by_param <- loc_rep_stats[, .(N=sum(n),
                                 grand_mean=sum(n*mu)/sum(n),
                                 var_within=sum(n*s^2)/sum(n),
                                 var_between=sum(n*(mu-grand_mean)^2)/sum(n)), by=param_id]
dt_by_param[, sigma.r := sqrt(var_within + var_between)]