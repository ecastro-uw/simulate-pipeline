# Visual Check 1: plot observed vs ensemble predictions with uncertainty
# For each location in a rep, plot the time series of simulated data and the 95% PI of the ensemble
# Report the p-value at time 0. Include key parameter values in the global title.

#TODO - Make flexible to type of output. The code currently assumes all draws are saved out
# and summarizes them to calc PI. If the output is already saved as summary stats, can skip
# this step and proceed straight to plotting.

#TODO - If using post-adjusted output, no need to re-calculate p-value as it will already be
# in the output
library(ggplot2)
library(patchwork)

# Which run version id do you want to examine?
version_id <- '20260101.01' 
# Do you want to plot pre- or post-adjusted predictions? ('pre' or 'adj' are valid options)
pred_type <- 'pre' 
# Which parameter id?
param <- 1
# Which batch id?
batch <- 2
# Which rep?
rep <- 7

# directory
root <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id)
dir <- file.path(root,'batched_output')

# parameters
params <- fread(file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id, 'params.csv'))
param_set <- params[param_id==param]
 
# load data and (pre-adjusted) estimates combine
pred_dt <- fread(paste0(dir,'/pred_', pred_type, '_p', param,'_b',batch,'.csv'))
obs_dt <- fread(paste0(dir,'/obs_p', param, '_b',batch,'.csv'))
dt <- merge(obs_dt, pred_dt, by=c('rep_id', 'location_id', 'time_id'), all=T)

# calculate p-value (proportion of draws <= obs)
n_draws <- unique(params$d) 
draw_cols <- paste0('draw_',1:n_draws)
dt$p_val <- rowSums(dt[, lapply(.SD, function(x) x <= y), .SDcols = draw_cols])/n_draws

# calculate PI
dt[, `:=`(mean = rowMeans(.SD, na.rm = T),
          LL = apply(.SD, 1, quantile, 0.025, na.rm = T),
          UL = apply(.SD, 1, quantile, 0.975, na.rm = T)), .SDcols = draw_cols]
dt <- dt[, (draw_cols) := NULL]

# Plot each location for a given rep
plot_one_loc <- function(data, loc_id, rep){
  plot_dt <- data[rep_id==rep & location_id==loc_id]
  
  ggplot(plot_dt) +
    geom_point(aes(x=time_id, y=y)) +
    geom_line(aes(x=time_id, y=y)) +
    geom_point(aes(x=time_id, y=mean), color='red') + 
    geom_line(aes(x=time_id, y=mean), color='red') + 
    annotate(geom='point', x=0,y=plot_dt[time_id==0,y]+params$theta, pch=21) +
    geom_errorbar(aes(x=time_id, y=mean, ymin=LL, ymax=UL), width=0.2, color='red') +
    theme_classic() +
    ggtitle(paste('p-value:', plot_dt[time_id==0, p_val]))
}


# Create list of plots for desired rep
loc_ids <- unique(dt$location_id)
plot_list <- lapply(loc_ids, function(loc) {
  plot_one_loc(data = dt, loc_id = loc, rep = rep)
})

# Combine plots into a grid
combined_plot <- wrap_plots(plot_list, ncol = 5, nrow = ceiling(param_set$L/5))

# Calculate rep-level p-value
rep_p <- mean(dt[rep_id==rep & time_id==0]$p_val)

# Add title
combined_plot + plot_annotation(
  title = paste0('L=', param_set$L, ', theta=',param_set$theta, ', sigma=', param_set$sigma.f, ', p.s=', param_set$p.s,
                 ', rep-level p=', rep_p),
  theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
)

