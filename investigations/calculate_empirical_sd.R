# Calculate empirical SD

library(data.table)
library(stringr)

version_id <- '20260306.01'

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
pdf(file.path(root,'realized_sigma.pdf'), width=7, height=5)
ggplot(summary_dt, aes(x=sigma.f,y=sigma.r)) +
  geom_smooth(method='lm', se=FALSE) +
  geom_point() +
  scale_x_continuous(name='sigma parameter', limits=c(0,10), breaks=seq(0,10,2)) + 
  scale_y_continuous(name='empirical sigma', limits=c(0,25), breaks=seq(0,25,5)) + 
  facet_grid(p_lab~L_lab) +
  theme_bw() +
  ggtitle('Relationship between noise parameters and empirical standard deviation \nDelta of last 3 pre-event timesteps (t=-3,-2,-1)')
dev.off()


# summarize the relationship between sigma and sigma.r when p.s=5
lm(sigma.r ~ sigma.f, data=summary_dt[p.s==1])

