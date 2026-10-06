# Summarize results by context
# After all jobs have finished running, this script should be run to create context-level summary tables and figures
# designed to compare results across contexts. Results fall into one of two categories:
# (1) Model performance measures - these include pre-adjusted empirical coverage, multiplier, mean PI width,
#     weighted interval score, and forecast skill. They are summarized in a table.
# (2) Signal detection -  these include median effect size across locations in the context, IQR of
#     the effect size, and the proportion of locations where the effect is in the hypothesized direction
#     (i.e. obs - pred is negative). Additionally includes a binomial p-value, which reflects the likelihood
#     of seeing as many or more locations reject the null assuming each location was no more likely than 
#     chance alone to reject.


library(scoringutils, lib.loc = '/ihme/homes/ems2285/lib_for_scoringutils')
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(colorspace)
library(dplyr)
library(tools)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# args
version_id <- '20261006.05'
#missing_contexts <- ''
inv_num <- 2

# dirs
root_dir <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/outputs',version_id)

# load context lookup file
context_lookup <- fread(file.path(root_dir,'inputs/context_lookup_table.csv'))
# allow for known missing contexts
#context_list <- context_lookup$context_id[-missing_contexts]
context_list <- context_lookup$context_id

# define context dimensions
if (inv_num==1){
  context_dims <- c('pop_cat', 'pol_cat')
} 
if (inv_num==2){
  context_dims <- c('state_abbrev')
}

# Load the location hierarchy
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)
counties <- merge(hierarchy[level==3, .(parent_id, location_id, location_name)],
                  hierarchy[level==2, .(location_id, state = location_name, state_abbrev = gsub('US-','',local_id))],
                  by.x='parent_id', by.y='location_id')
counties[, full_name := paste0(location_name, ', ', state_abbrev)]


### PART 1 - MODEL PERFORMANCE MEASURES ###

# Calculate a weighted interval score for a given context_id and model.
# Evaluate performance for the week prior to the event (t=-1)
pi_probs <- c(0.01, 0.025, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45,
              0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975, 0.99)
pi_names <- paste0('q', pi_probs * 100)

calc_wis <- function(context, model){
  
  # load prediction quantiles
  if (model=='adj_ensemble') {
    preds <- fread(paste0(root_dir,'/batched_output/pred_adj_M2_context_',context,'.csv'))
  } else if (model=='unadj_ensemble') {
    preds <- fread(paste0(root_dir,'/batched_output/pred_pre_context_',context,'.csv'))[time_id==-1]
  } else if (model=='unadj_naive') {
    preds <- fread(paste0(root_dir,'/batched_output/candidate_mods_context_',context,'.csv'))[model=='model_1' & time_id==-1,]
  } else {
    stop(paste(model, 'is not a valid model for the function calc_wis().'))
  }
  
  # load observed value
  obs <- fread(paste0(root_dir,'/batched_output/obs_context_',context,'.csv'))[time_id==-1,.(location_id, y)]
  
  # calculate WIS
  scores <- wis(observed = obs$y, 
                predicted = as.matrix(preds[, .SD, .SDcols = pi_names]),
                quantile = pi_probs)
  
  return(scores)
}

# Calculate forecast skill (skill = 1 - (wis/wis_baseline))
calc_skill <- function(context, adj=TRUE){
  if (adj==TRUE){
    numerator <- sum(calc_wis(context, model='adj_ensemble'))
  } else {
    numerator <- sum(calc_wis(context, model='unadj_ensemble'))
  }
  denominator <- sum(calc_wis(context, model='unadj_naive'))
  
  skill <- 1 - (numerator / denominator)
  return(skill)
}

# Compile into a table with one row per context id
build_performance_table <- function(context){
  temp_dt <- data.table(context_id  = context,
                        unadj_skill = round(calc_skill(context, adj=F),2),
                        adj_skill   = round(calc_skill(context, adj=T),2))
  return(temp_dt)
}
performance_dt <- rbindlist(lapply(context_list, build_performance_table))

# Add context info
performance_dt <- merge(context_lookup[,.SD, .SDcols = c('context_id', 'mandate_num', 'mandate_type', context_dims, 'N')],
                        performance_dt, by='context_id')

# Save table
fwrite(performance_dt, paste0(root_dir,'/model_performance_summary.csv'))


### PART 2 - SIGNAL DETECTION ###

### (A) Compile results table
# Median effect size, IQR of the effect size, % of locations with effect in hypothesized direction
calc_eff_size <- function(context){
  
  # load predictions
  pi <- fread(paste0(root_dir,'/batched_output/pred_adj_context_',context,'.csv'))[time_id==0, .(location_id, q2.5, q50, q97.5, p_val)]
  
  # load observed value
  obs <- fread(paste0(root_dir,'/batched_output/obs_context_',context,'.csv'))[time_id==0,.(location_id, y)]
  
  # combine
  dt <- merge(obs,pi, by='location_id')
  
  # calculate difference between observed and median of PI
  dt[, eff_size := y - q50]
  
  # calculate p-value across all locations
  k <- nrow(dt[p_val < 0.05]) #number of sig locs
  binom_p <- round(pbinom(k,nrow(dt),0.05,lower.tail=F),2)
  
  # calculate median and IQR of effect size across locations
  temp_dt <- data.table(context_id = context,
                        eff_size_median = round(median(dt$eff_size),2),
                        eff_size_Q1 = round(quantile(dt$eff_size, 0.25),1),
                        eff_size_Q3 = round(quantile(dt$eff_size, 0.75),1),
                        eff_size_IQR = round(IQR(dt$eff_size),2),
                        num_sig_locs = k,
                        eff_pct_neg = round((sum(dt$eff_size<0)/nrow(dt))*100,1),
                        pct_sig_p = round((sum(dt$p_val<0.05)/nrow(dt))*100, 1),
                        binom_p = binom_p
  )
  
  temp_dt[, eff_size_final := paste0(eff_size_median,' (',eff_size_Q1,', ',eff_size_Q3,')')]
  
  return(temp_dt)
}

effect_size_dt <- rbindlist(lapply(context_list, calc_eff_size))
effect_size_dt <- merge(context_lookup[,.SD, .SDcols = c('context_id', 'mandate_num', 'mandate_type', context_dims, 'N')],
                        effect_size_dt, by='context_id')
# Save table
fwrite(effect_size_dt, paste0(root_dir,'/effect_size_summary.csv'))


### (B) Meta Analysis
calc_eff_size_meta <- function(context){
  
  # load thetas
  thetas <- fread(paste0(root_dir,'/batched_output/thetas_context_',context,'.csv'))
  draw_cols <- grep("^draw_", names(thetas), value = TRUE)
  # exponentiate to yield Relative Risks and combine into one long vector
  all_draws <- exp(unlist(thetas[, ..draw_cols])) #vector of RRs: N locs x 1000 draws per loc
  
  point_est <- median(all_draws)
  ci_95 <- quantile(all_draws, c(0.025, 0.975))
  
  temp_dt <- data.table(context_id = context,
                        RR_median = round(point_est,2),
                        RR_lower = ci_95[1],
                        RR_upper = ci_95[2],
                        CI_RR = paste0('(',round(ci_95[1],2),', ', round(ci_95[2],2),')'),
                        direction = ifelse(ci_95[2] < 1, 'decrease', ifelse(ci_95[1] > 1, 'increase', 'indeterminate'))
  )
  return(temp_dt)
}
effect_size_dt2 <- rbindlist(lapply(context_list, calc_eff_size_meta))
effect_size_dt2 <- merge(context_lookup[,.SD, .SDcols = c('context_id', 'mandate_num', 'mandate_type', context_dims, 'N')],
                         effect_size_dt2, by='context_id')
# Save table
fwrite(effect_size_dt2, paste0(root_dir,'/effect_size_meta.csv'))

