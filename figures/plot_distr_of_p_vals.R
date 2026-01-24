# Visual Check 2: p-value distribution
# For a given parameter set, plot the distribution of p-values across all reps

library(ggplot2)
library(patchwork)

# Which run version id do you want to examine?
version_id <- '20260109.01' 
# Do you want to plot pre- or post-adjusted ensemble predictions? ('pre' or 'adj' are valid options)
pred_type <- 'pre' 


# directory
root <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id)
dir <- file.path(root,'batched_output')

# parameters
params <- fread(file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id, 'params.csv'))


# define a function to process one batch's worth of reps
prep_one_batch <- function(batch, pred_type, param, n_draws){
  
  if(pred_type=='pre'){
    # load obs
    obs_dt <- fread(paste0(dir,'/obs_p', param, '_b', batch, '.csv'))
    # load preds
    file <- paste0(dir,'/pred_pre_p', param,'_b', batch,'.csv')
    dt <- fread(file)[time_id==0]
    # combine
    dt <- merge(obs_dt, dt, by=c('rep_id', 'location_id', 'time_id'))
    # calc p
    draw_cols <- paste0('draw_',1:n_draws)
    dt$p_val <- rowSums(dt[, lapply(.SD, function(x) x <= y), .SDcols = draw_cols])/n_draws
    dt <- dt[, (draw_cols) := NULL]
  } else {
    file <- paste0(dir,'/pred_adj_p', param,'_b', batch,'.csv')
    dt <- fread(file)[time_id==0, .(rep_id, location_id, time_id, p_val)]
  }
  
  # calculate the mean p-value by rep
  dt_by_rep <- dt[, list(p_val = mean(p_val)), by=rep_id] 
  dt_by_rep[, batch_id := batch]
  return(dt_by_rep)
}



prep_one_param_id <- function(param, pred_type){
  # get parameters associated with the param_id
  param_set <- params[param_id==param]
  n_draws <- param_set$d
  # prep and combine p values from all reps
  dt <- rbindlist(lapply(1:param_set$B, prep_one_batch, pred_type=pred_type, param, n_draws))
  dt[, L:= param_set$L]
  dt[, theta:=param_set$theta]
  return(dt)
}


# which param_ids correspond with the runs I want to plot?
param_ids <- params[theta==0.16 & p.s==0.5 & L %in% c(10, 20, 30), param_id]
#param_ids <- params$param_id
p_vals_dt <- rbindlist(lapply(param_ids, prep_one_param_id, pred_type='pre'))

# Plot
ggplot() +
  geom_density(data=p_vals_dt, aes(x=p_val, color=as.factor(L))) +
  geom_vline(xintercept=0.05, linetype='dashed') +
  facet_wrap(~theta) +
  #ggtitle('theta=0.16, p.s=0.5, sigma.f=0.1, sigma.s=0.1') +
  theme_bw() +
  scale_color_discrete(name="L") +
  scale_x_continuous(limits=c(0,0.15))
  



# If plotting just one param id, but comparing pre- and post-adjusted p-values
p_pre <- rbindlist(lapply(1:param_set$B, prep_one_batch, pred_type='pre', param, n_draws))
p_post <- rbindlist(lapply(1:param_set$B, prep_one_batch, pred_type='adj', param, n_draws))

ggplot() +
  geom_density(data=p_pre, aes(x=p_val)) +
  geom_density(data=p_post, aes(x=p_val), color='red') +
  geom_vline(xintercept = 0.05, linetype='dashed') +
  theme_classic()

