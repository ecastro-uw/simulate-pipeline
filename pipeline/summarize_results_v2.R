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
library(metafor)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# args
version_id <- '20261013.04'
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
  # p = Pr(X>=k)
  # lower.tail=F tests Pr(X>k) so first arg of pbinom must be k-1
  k <- nrow(dt[p_val < 0.05]) #number of sig locs
  binom_p <- round(pbinom(k-1,nrow(dt),0.05,lower.tail=F),2)
  
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
# Within each context, each county is treated as a "study". theta = y - y_hat is a log RR,
# where y = log((weekly visits + 0.5) / mean weekly baseline visits); RR = exp(theta).
# For each county: yi = mean of its theta draws, vi = variance of its theta draws.
# Pooling is done on the log scale and results are exponentiated to RRs:
#   (a) Inverse-variance (fixed/equal effect): weights 1/vi
#   (b) Random effects (REML; DerSimonian-Laird fallback if REML fails): weights 1/(vi + tau^2)
# Population is not used in the weights; it is reported, and used in a diagnostic that checks
# whether smaller counties have wider uncertainty (cor_logpop_sd < 0 is expected).
# Contexts with fewer than min_counties counties are not estimated.

min_counties <- 10

# Load county populations (output of data_processing/prep_population.R)
pop_path <- '/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/USA_counties/population.csv'
pop_dt <- unique(fread(pop_path)[, .(location_id, pop)], by='location_id')

# classify direction of effect from a CI on the RR scale
get_direction <- function(lower, upper){
  ifelse(is.na(lower) | is.na(upper), NA_character_,
         ifelse(upper < 1, 'decrease', ifelse(lower > 1, 'increase', 'indeterminate')))
}
fmt_ci <- function(lower, upper){
  ifelse(is.na(lower), NA_character_, paste0('(',round(lower,2),', ', round(upper,2),')'))
}

calc_eff_size_meta <- function(context){
  
  # load thetas (one row per location, one column per draw)
  thetas <- fread(paste0(root_dir,'/batched_output/thetas_context_',context,'.csv'))
  draw_cols <- grep("^draw_", names(thetas), value = TRUE)
  
  # attach population to each location
  thetas <- merge(thetas, pop_dt, by='location_id', all.x=TRUE)
  missing_pop <- thetas[is.na(pop), location_id]
  if (length(missing_pop) > 0){
    warning(paste0('Context ', context, ': dropping ', length(missing_pop),
                   ' location(s) with no population data: ', paste(missing_pop, collapse=', ')))
    thetas <- thetas[!is.na(pop)]
  }
  
  # per-county effect size (log RR) and within-county variance from the draws
  theta_mat <- as.matrix(thetas[, ..draw_cols])
  county_dt <- data.table(context_id  = context,
                          location_id = thetas$location_id,
                          pop         = thetas$pop,
                          yi          = rowMeans(theta_mat, na.rm = TRUE),
                          vi          = apply(theta_mat, 1, var, na.rm = TRUE))
  county_dt[, sdi := sqrt(vi)]
  k <- nrow(county_dt)
  
  # diagnostic: do smaller counties have wider uncertainty? (expect negative)
  cor_logpop_sd <- if (k >= 3) cor(log(county_dt$pop), county_dt$sdi, method = 'spearman') else NA_real_
  
  # template row (all estimates NA)
  temp_dt <- data.table(context_id = context,
                        N_locs = k,
                        total_pop = sum(county_dt$pop),
                        cor_logpop_sd = round(cor_logpop_sd, 2),
                        RR_IV = NA_real_, RR_IV_lower = NA_real_, RR_IV_upper = NA_real_,
                        CI_RR_IV = NA_character_, direction_IV = NA_character_,
                        RR_RE = NA_real_, RR_RE_lower = NA_real_, RR_RE_upper = NA_real_,
                        CI_RR_RE = NA_character_, direction_RE = NA_character_,
                        PI_RE_lower = NA_real_, PI_RE_upper = NA_real_,
                        tau2 = NA_real_, I2 = NA_real_, Q_pval = NA_real_,
                        re_method = NA_character_, note = NA_character_)
  
  # do not estimate for small contexts
  if (k < min_counties){
    warning(paste0('Context ', context, ': only ', k, ' counties (< ', min_counties, '); not estimated.'))
    temp_dt[, note := paste('fewer than', min_counties, 'counties')]
    return(list(summary = temp_dt, county = county_dt))
  }
  
  # (a) inverse-variance (equal/fixed effect)
  fit_iv <- rma(yi = yi, vi = vi, data = county_dt, method = 'EE')
  temp_dt[, `:=`(RR_IV       = round(exp(as.numeric(fit_iv$beta)), 3),
                 RR_IV_lower = exp(fit_iv$ci.lb),
                 RR_IV_upper = exp(fit_iv$ci.ub))]
  
  # (b) random effects: REML, falling back to DerSimonian-Laird if REML fails to converge
  re_method <- 'REML'
  fit_re <- tryCatch(rma(yi = yi, vi = vi, data = county_dt, method = 'REML'),
                     error = function(e) NULL)
  if (is.null(fit_re)){
    warning(paste0('Context ', context, ': REML did not converge; using DerSimonian-Laird.'))
    re_method <- 'DL'
    fit_re <- rma(yi = yi, vi = vi, data = county_dt, method = 'DL')
  }
  pred_re <- predict(fit_re, transf = exp) # pooled RR, CI, and 95% prediction interval
  temp_dt[, `:=`(RR_RE       = round(pred_re$pred, 3),
                 RR_RE_lower = pred_re$ci.lb,
                 RR_RE_upper = pred_re$ci.ub,
                 PI_RE_lower = pred_re$pi.lb,
                 PI_RE_upper = pred_re$pi.ub,
                 tau2        = fit_re$tau2,
                 I2          = round(fit_re$I2, 1),
                 Q_pval      = fit_re$QEp,
                 re_method   = re_method)]
  
  temp_dt[, `:=`(CI_RR_IV     = fmt_ci(RR_IV_lower, RR_IV_upper),
                 direction_IV = get_direction(RR_IV_lower, RR_IV_upper),
                 CI_RR_RE     = fmt_ci(RR_RE_lower, RR_RE_upper),
                 direction_RE = get_direction(RR_RE_lower, RR_RE_upper))]
  
  return(list(summary = temp_dt, county = county_dt))
}
meta_results <- lapply(context_list, calc_eff_size_meta)
effect_size_dt2 <- rbindlist(lapply(meta_results, `[[`, 'summary'))
county_var_dt   <- rbindlist(lapply(meta_results, `[[`, 'county'))
effect_size_dt2 <- merge(context_lookup[,.SD, .SDcols = c('context_id', 'mandate_num', 'mandate_type', context_dims, 'N')],
                         effect_size_dt2, by='context_id')

# Diagnostic outputs: per-county within-county uncertainty vs population
fwrite(county_var_dt, paste0(root_dir,'/county_theta_variance.csv'))
p_sd <- ggplot(county_var_dt, aes(x = pop, y = sdi)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_smooth(method = 'loess', se = TRUE, color = 'blue') +
  scale_x_log10(name = 'County population (log scale)', labels = scales::comma) +
  scale_y_continuous(name = 'SD of theta draws (log RR)') +
  ggtitle('Within-county uncertainty vs. population') +
  theme_bw()
ggsave(paste0(root_dir,'/theta_sd_vs_pop.pdf'), p_sd, width = 7, height = 5)

# Save table
fwrite(effect_size_dt2, paste0(root_dir,'/effect_size_meta.csv'))

