
# Coverage adjustment plots

# Which run version id do you want to examine?
version_id <- '20260113.01' 

# directory
root <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id)
dir <- file.path(root,'batched_output')

# parameters
params <- fread(file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id, 'params.csv'))

# combine results for all param_ids
load_one_file <- function(file){
  param <- as.numeric(str_extract(file, "(?<=_p)\\d+"))
  dt <- fread(file)
  dt[, param_id := param]
  dt[, batch_id := as.numeric(str_extract(file, "(?<=_b)\\d+"))]
}
list_of_files <- list.files(dir, pattern=paste0("coverage_"), full.names = T)
all_files <- rbindlist(lapply(list_of_files, load_one_file))

# add parameter values
all_files <- merge(all_files, params[, .(param_id, theta, L)], by='param_id')
all_files[, L_lab := factor(L, 
                            levels=unique(all_files$L),
                            labels=paste('L =', unique(all_files$L)))]
all_files[, theta_lab := factor(theta, 
                            levels=unique(all_files$theta),
                            labels=paste('theta =', unique(all_files$theta)))]

# create an annotation file showing the number of reps with multiplier<1
annotations <- all_files[, .(pct_shrink = (sum(multiplier<1)/.N)*100), by=c('L_lab', 'theta_lab')]
annotations[, label := paste0(pct_shrink,'% <1')]
annotations$x <- 1.7
annotations$y <- 4

# plot multiplier
p1 <- ggplot(all_files) +
  geom_density(aes(x=multiplier)) +
  geom_label(data = annotations, aes(x=x, y=y, label=label)) +
  scale_y_continuous(name="density") +
  facet_grid(L_lab~theta_lab) +
  theme_bw() +
  ggtitle('Distribution of multiplier across simulation runs')

# compare pre- and post-adjusted coverage
plot2_dt <- melt(all_files,
                 id.vars = c('L_lab', 'theta_lab'),
                 measure.vars=c('coverage_pre', 'coverage_post'),
                 value.name='coverage')
plot2_dt[, variable := gsub('coverage_','', variable)]

p2 <- ggplot(plot2_dt) +
  geom_density(aes(x=coverage, color=variable)) +
  facet_grid(L_lab~theta_lab) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  ggtitle('Distribution of coverage across simulation runs')

p3 <- ggplot(all_files) +
  geom_point(aes(x=coverage_pre, y=coverage_post), alpha=0.4) +
  geom_vline(xintercept = 0.95, linetype='dashed') +
  geom_hline(yintercept = 0.95, linetype='dashed') +
  facet_grid(L_lab~theta_lab) +
  theme_bw()
  

pdf(file.path(root,'coverage_plots.pdf'), width=11, height=6)
p1
p2
p3
dev.off()



## -- Investigate large multipliers -- ##
sort(all_files[theta==0.4 & L==10]$multiplier)[995:1000]
all_files[theta==0.4 & L==10 & multiplier>1.68] # large multipliers
all_files[theta==0.4 & L==10 & coverage_pre < 0.7] # low pre-adj coverage

# Pre- or post-adjusted?
pred_type <- 'pre' 
# Which parameter id?
param <- 2
# Which batch?
batch <- 6
# Which rep?
rep <- 12  #12, 14, 30

# parameters
param_set <- parameters[param_id==param]

# multiplier
multiplier <- all_files[param_id==param & batch_id==batch & rep_id==rep]$multiplier

# load data 
obs_dt <- fread(paste0(dir,'/obs_p', param, '_b',batch,'.csv'))[rep_id==rep]
obs_dt[, model := 'obs']
setnames(obs_dt, 'y', 'mean')

# load ensemble predictions 
pred_dt <- fread(paste0(dir,'/pred_', pred_type, '_p', param,'_b',batch,'.csv'))[rep_id==rep]

pred_summary <- copy(pred_dt)
pred_summary[, `:=`(mean = rowMeans(.SD, na.rm = T),
               LL = apply(.SD, 1, quantile, 0.025, na.rm = T),
               UL = apply(.SD, 1, quantile, 0.975, na.rm = T)), .SDcols = draw_cols]
pred_summary <- pred_summary[, (draw_cols) := NULL]

# combine
dt <- rbind(obs_dt, pred_summary, fill=T)


# Plot each location
plot_one_loc <- function(data, loc_id){
  
  plot_dt <- data[location_id==loc_id]
  
  ggplot(plot_dt) +
    geom_point(aes(x=time_id, y=mean, color=model)) +
    geom_line(data=plot_dt[model!='obs'], aes(x=time_id, y=mean, color=model)) +
    geom_errorbar(data=plot_dt[model=='ensemble'],aes(x=time_id, y=mean, ymin=LL, ymax=UL), width=0.2, alpha=1, color='pink') +
    scale_y_continuous(name='y') +
    theme_classic()
}

# Create list of plots for desired rep
loc_ids <- unique(dt$location_id)
plot_list <- lapply(loc_ids, function(loc) {
  plot_one_loc(data = dt, loc_id = loc, rep = rep)
})

# Combine plots into a grid
combined_plot <- wrap_plots(plot_list, ncol = 5, nrow = ceiling(param_set$L/5))



# combine results for a single param_id
one_param <- 20
list_of_files <- list.files(dir, pattern=paste0("coverage_p",one_param,"_"), full.names = T)
one_param_dt <- rbindlist(lapply(list_of_files, fread))