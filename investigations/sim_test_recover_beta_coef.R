# Check if beta was recovered

# Which run version id do you want to examine?
version_id <- '20260325.01'


# Directory
root <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/simulations', version_id)
dir <- file.path(root,'batched_output')

# Load param file
params <- fread(paste0(root,'/params.csv'))

# load data and preds
calc_beta <- function(p){
  
  pred_dt <- fread(paste0(dir,'/pred_adj_p',p,'_b1.csv'))
  obs_dt <- fread(paste0(dir,'/obs_p',p,'_b1.csv'))
  
  if(params[param_id==p]$data_model=='simple_lm'){
    obs_dt[, lagged_x := shift(x), by=c('rep_id', 'location_id')]
    
    dt <- merge(pred_dt[, .(rep_id, location_id, time_id, yhat = mean)],
                 obs_dt[, .(rep_id, location_id, time_id, lagged_x)], by=c('rep_id', 'location_id', 'time_id'))
    
    results <- dt %>%
      group_by(rep_id) %>%
      summarise(
        beta0 = coef(lm(yhat ~ lagged_x))[["(Intercept)"]],
        beta1 = coef(lm(yhat ~ lagged_x))[["lagged_x"]],
        .groups = "drop"
      )
    
    results$true_beta0 <- params[param_id==p, beta0]
    results$true_beta1 <- params[param_id==p, beta1]
  }
  
  if(params[param_id==p]$data_model=='ar_simple_lm'){
    obs_dt[, lagged_y := shift(y), by=c('rep_id', 'location_id')]
    obs_dt[, lagged_x := shift(x), by=c('rep_id', 'location_id')]
    
    dt <- merge(pred_dt[, .(rep_id, location_id, time_id, yhat = mean)],
                obs_dt[, .(rep_id, location_id, time_id, lagged_y, lagged_x)], by=c('rep_id', 'location_id', 'time_id'))
    
    results <- dt %>%
      group_by(rep_id) %>%
      summarise(
        beta0 = coef(lm(yhat ~ lagged_y + lagged_x))[["(Intercept)"]],
        beta1 = coef(lm(yhat ~ lagged_y + lagged_x))[["lagged_y"]],
        beta2 = coef(lm(yhat ~ lagged_y + lagged_x))[["lagged_x"]],
        .groups = "drop"
      )
    
    results$true_beta0 <- params[param_id==p, beta0]
    results$true_beta1 <- params[param_id==p, beta1]
    results$true_beta2 <- params[param_id==p, beta2]
  }
  
  results$t <- params[param_id==p, t]
  results$sigma <- params[param_id==p, sigma]
  
  return(results)
}

all_betas <- rbindlist(lapply(unique(params$param_id), calc_beta))
all_betas[, t_label := factor(paste0('T=',t),
                                 levels=c('T=8','T=15'))]
all_betas[, sigma_label := as.factor(paste0('sigma=',sigma)]

ggplot(all_betas) +
  geom_histogram(aes(x=beta1), bins=12) +
  geom_vline(xintercept=unique(all_betas$true_beta1)) +
  theme_classic() +
  facet_grid(t_label ~ sigma_label)

ggplot(results) +
  geom_histogram(aes(x=beta2), bins=12) +
  geom_vline(xintercept=unique(results$true_beta2)) +
  theme_classic()
