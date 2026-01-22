# Visual Check 3: Candidate Models
# For each location in a rep, plot the time series of simulated data along with the mean pred
# from the naive flat, naive slope, and ensemble models. Report the fitted ensemble weights.

library(ggplot2)
library(patchwork)

# Which run version id do you want to examine?
version_id <- '20260113.01' 
# Do you want to plot pre- or post-adjusted ensemble predictions? ('pre' or 'adj' are valid options)
pred_type <- 'pre' 
# Which parameter id?
param <- 2
# Which batch?
batch <- 6
# Which rep?
rep <- 12

# directory
root <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id)
dir <- file.path(root,'batched_output')

# parameters
params <- fread(file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id, 'params.csv'))
param_set <- params[param_id==param]

# draws
draw_cols <- paste0('draw_',1:param_set$d)

# load the candidate model predictions and summarize
naive_dt <- fread(paste0(dir,'/candidate_mods_p',param,'_b',batch,'.csv'))
naive_dt[, `:=`(mean = rowMeans(.SD, na.rm = T),
                LL = apply(.SD, 1, quantile, 0.025, na.rm = T),
                UL = apply(.SD, 1, quantile, 0.975, na.rm = T)), .SDcols = draw_cols]
naive_dt <- naive_dt[, (draw_cols) := NULL]

# load data 
obs_dt <- fread(paste0(dir,'/obs_p', param, '_b',batch,'.csv'))
obs_dt[, model := 'obs']
setnames(obs_dt, 'y', 'mean')

# load ensemble predictions and summarize
pred_dt <- fread(paste0(dir,'/pred_', pred_type, '_p', param,'_b',batch,'.csv'))
pred_dt[, model := 'ensemble']
pred_dt[, `:=`(mean = rowMeans(.SD, na.rm = T),
               LL = apply(.SD, 1, quantile, 0.025, na.rm = T),
               UL = apply(.SD, 1, quantile, 0.975, na.rm = T)), .SDcols = draw_cols]
pred_dt <- pred_dt[, (draw_cols) := NULL]

# combine
dt <- rbind(naive_dt, obs_dt, fill=T)
dt <- rbind(dt, pred_dt)

# load ensemble weights
weights_dt <- fread(paste0(dir,'/ens_weights_p',param,'_b',batch,'.csv'))

# Plot each location for a given rep
plot_one_loc <- function(data, loc_id, rep){
  
  plot_dt <- data[rep_id==rep & location_id==loc_id]
  
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

# get weight for naive slope
p_s <- round(weights_dt[rep_id==rep & model=='naive_slope_2']$weight,2)

# Add title
combined_plot +
  plot_annotation(
    title = paste0('L=', param_set$L, ', theta=',param_set$theta, ', sigma=', param_set$sigma.f, ', p.s=', param_set$p.s,
                   ', fitted p_s=', p_s),
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16))) +
  plot_layout(
    guides='collect'
  )


