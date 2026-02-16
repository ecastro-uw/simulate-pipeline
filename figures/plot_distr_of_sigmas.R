# Visual Check 4: Distribution of sigma
# For a given parameter set, plot the distribution of sigma_f and sigma_s across all reps.
# Plot the distribution across different values of L

# Which run version id do you want to examine?
version_id <- '20260109.01' 

# directory
root <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id)
dir <- file.path(root,'batched_output')

# parameters
params <- fread(file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id, 'params.csv'))

# define a function to process one batch's worth of reps
prep_one_batch <- function(batch, param){
  
  one_batch_dt <- fread(paste0(dir,'/sigmas_p', param, '_b', batch, '.csv'))[time_id==0]
  one_batch_wide <- dcast(one_batch_dt, rep_id ~ model, value.var = "sigma")
  setnames(one_batch_wide, old = c("naive_flat_1", "naive_slope_2"), new = c("sigma.f", "sigma.s"))
  one_batch_wide[, batch_id := batch]

  return(one_batch_wide)
}

# define process to combine all batches
prep_one_param_id <- function(param){
  # get parameters associated with the param_id
  param_set <- params[param_id==param]

  # prep and combine sigma values from all reps
  all_batches_dt <- rbindlist(lapply(1:param_set$B, prep_one_batch, param))
  all_batches_dt[, L:= param_set$L]
  return(all_batches_dt)
}

# which param_ids correspond with the runs I want to plot?
param_ids <- params[theta==0.16 & p.s==0.5 & L %in% c(10, 20, 30), param_id]
#param_ids <- params$param_id
sigmas_dt <- rbindlist(lapply(param_ids, prep_one_param_id))

# Plot distribution of sigma.f at time 0 for varying levels of L
ggplot() +
  geom_density(data=sigmas_dt, aes(x=sigma.f, color=as.factor(L))) +
  geom_vline(xintercept=0.1, linetype='dashed') +
  ggtitle('theta=0.22, p.s=0.5, sigma.f=0.1, sigma.s=0.1') +
  theme_bw() +
  scale_color_discrete(name="L") +
  scale_x_continuous(limits=c(0.05, 0.16))
# Plot distribution of sigma.s at time 0 for varying levels of L
ggplot() +
  geom_density(data=sigmas_dt, aes(x=sigma.s, color=as.factor(L))) +
  geom_vline(xintercept=0.1, linetype='dashed') +
  ggtitle('theta=0.22, p.s=0.5, sigma.f=0.1, sigma.s=0.1') +
  theme_bw() +
  scale_color_discrete(name="L") +
  scale_x_continuous(limits=c(0.05, 0.16))

# Scatter sigma.f vs sigma.s at time 0 for a single value of L
plot_dt <- merge(sigmas_dt, p_vals_dt, by=c('batch_id','rep_id','L'))
plot_dt[, sig := ifelse(p_val < 0.05, 1, 0)]
ggplot() +
  geom_point(data=plot_dt, aes(x=sigma.f, y=sigma.s, color=as.factor(sig)), alpha=0.4) +
  scale_x_continuous(limits=c(0.06,0.17)) +
  scale_y_continuous(limits=c(0.06,0.17)) +
  scale_color_discrete(name="p<0.05") +
  facet_wrap(~L)





# For each L, plot a 5x2 grid showing the distribution of sigma at each time step
# define a function to process one batch's worth of reps
prep_one_batch <- function(batch, param){
  one_batch_dt <- fread(paste0(dir,'/sigmas_p', param, '_b', batch, '.csv'))
  one_batch_dt[, batch_id := batch]
}

# define process to combine all batches
prep_one_param_id <- function(param){
  # get parameters associated with the param_id
  param_set <- params[param_id==param]
  
  # prep and combine sigma values from all reps
  all_batches_dt <- rbindlist(lapply(1:param_set$B, prep_one_batch, param))
  return(all_batches_dt)
}

# which param_id corresponds with the runs I want to plot?
param <- params[p.s==0.5 & L==100, param_id]
sigmas_dt <- rbindlist(lapply(param, prep_one_param_id))

# plot
ggplot(sigmas_dt, aes(x=sigma)) +
  geom_density(fill="steelblue", alpha=0.4) +
  geom_vline(xintercept=0.1, linetype='dashed') +
  facet_grid(model ~ time_id,
             labeller = labeller(time_id = function(x) paste("Time",x))) +
  theme_minimal() +
  labs(x="Sigma", y="Density") +
  scale_x_continuous(breaks=c(0.05,0.1,0.15), limits=c(0.05,0.15)) +
  ggtitle('L=100')
