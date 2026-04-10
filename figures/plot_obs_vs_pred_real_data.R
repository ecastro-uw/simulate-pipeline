# Visual Check 1: plot observed vs ensemble predictions with uncertainty
# For each location in a rep, plot the time series of simulated data and the 95% PI of the ensemble
# Report the p-value at time 0. 

library(ggplot2)
library(patchwork)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# Which run version id do you want to examine?
version_id <- '20260330.01' 
# Do you want to plot pre- or post-adjusted predictions? ('pre' or 'adj' are valid options)
pred_type <- 'adj' 
# Which group id?
group_id <- 3

# Load the location hierarchy
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)

# Directory
root <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/outputs', version_id)
dir <- file.path(root,'batched_output')

# load data and preds
obs_dt <- fread(paste0(dir,'/obs_group_', group_id,'.csv'))
pred_dt <- fread(paste0(dir,'/pred_', pred_type, '_group_', group_id,'.csv'))

# Plot each location
plot_one_loc <- function(data, preds, loc_id){
  # prep data for plotting
  plot_obs_dt <- data[location_id==loc_id]
  plot_pred_dt <- preds[location_id==loc_id]
  plot_dt <- merge(plot_obs_dt, plot_pred_dt[,.(location_id, time_id, mean, LL = q2.5, UL = q97.5, p_val)], 
                   by=c('location_id', 'time_id'), all.x=T)
  
  # get location name
  title <- paste0(hierarchy[location_id==loc_id, location_name], ', WA')
  
  p1 <- ggplot(plot_dt) +
    geom_point(aes(x=time_id, y=y)) +
    geom_line(aes(x=time_id, y=y)) +
    geom_errorbar(data=plot_dt, aes(x=time_id, y=mean, ymin=LL, ymax=UL), width=0.4, color='red') +
    #geom_errorbar(data=plot_dt[time_id==0], aes(x=time_id, y=mean, ymin=LL, ymax=UL), width=0.4, color='red') +
    theme_classic() +
    ggtitle(title)
    #ggtitle(paste0(title,' (p=', plot_dt[time_id==0, p_val],')'))
  
  print(p1)
}


# Create list of plots for desired rep
loc_ids <- unique(pred_dt$location_id)
#plot_list <- lapply(loc_ids, function(loc) {
#  plot_one_loc(data = obs_dt, preds = pred_dt, loc_id = loc)
#})

# Combine plots into a grid
#combined_plot <- wrap_plots(plot_list, ncol = 5)

pdf(paste0(root,'/obs_vs_pred_group_',group_id,'.pdf'), width=7, height=5)
for(loc in loc_ids){
  plot_one_loc(obs_dt, pred_dt, loc_id = loc)
}
dev.off()
