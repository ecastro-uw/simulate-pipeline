# Summarize results by context
# After all jobs have finished running, this script should be run to create summary tables and figures
# designed to compare results across contexts. Results fall into one of two categories:
# (1) Model performance measures - these include pre-adjusted empirical coverage, multiplier, mean PI width,
#     weighted interval score, and forecast skill. They are summarized in a table.
# (2) Signal detection -  these include median effect size across locations in the context, IQR of
#     the effect size, and the proportion of locations where the effect is in the hypothesized direction
#     (i.e. obs - pred is negative). Additionally includes a binomial p-value, which reflects the likelihood
#     of seeing as many or more locations reject the null assuming each location was no more likely than 
#     chance alone to reject.

#.libPaths(c("/ihme/homes/ems2285/rpackages/", .libPaths()))
install.packages('scoringutils',
                 lib='/ihme/homes/ems2285/rpackages/')

#remotes::install_github("cmu-delphi/covidcast", ref = "main",
#                        subdir = "R-packages/evalcast")

library(data.table)
library(ggplot2)
library(dplyr)

# args
version_id <- '20260412.02'

# dirs
root_dir <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/outputs/',version_id)

# load context lookup file
context_lookup <- fread(file.path(root_dir,'inputs/context_lookup_table.csv'))

### PART 1 - MODEL PERFORMANCE MEASURES ###

# For each context, calculate the weighted interval score using the adjusted ensemble
# prediction for the week prior to the event (t=-1)
calc_wis <- function(context, model){
  
  # load prediction intervals
  if(model=='ensemble'){
    pi <- fread(paste0(root_dir,'/batched_output/pred_adj_context_',context,'.csv'))[time_id==-1, .(location_id, q2.5, q50, q97.5)]
  } else {
    pi <- fread(paste0(root_dir,'/batched_output/candidate_mods_context_',context,'.csv'))[model=='model_1' & time_id==-1,
                                                                                           .(location_id, q2.5, q50, q97.5)]
  }
  
  # load observed value
  obs <- fread(paste0(root_dir,'/batched_output/obs_context_',context,'.csv'))[time_id==-1,.(location_id, y)]
  
  # combine
  dt <- merge(obs,pi, by='location_id')
  
  # calculate WIS
  #score <- wis(observed = dt$y, 
  #             predicted = dt[, .(q2.5, q0.5, q97.5)],
  #             quantile = c(0.025, 0.5, 0.975))
  
  dt[, below := ifelse(y<q2.5,1,0)]
  dt[, above := ifelse(y>q97.5,1,0)]
  dt[, is := (q97.5 - q2.5) + (2/0.05)*(q2.5 - y)*below + (2/0.05)*(y - q97.5)*above]
  
  return(score)
}

# TODO - Calculate forecast skill (skill = 1 - (wis_ens/wis_naive))
calc_skill <- function(context){
  ens_wis <- calc_wis(context, model='ensemble')
  naive_wis <- calc_wis(context, model='model_1')
}


# Compile other measures of model performance
get_performance_stats <- function(context){
  # empirical coverage and multiplier
  coverage <- fread(paste0(root_dir,'/batched_output/coverage_context_',context,'.csv'))
  
  # mean pi width
  pi <- fread(paste0(root_dir,'/batched_output/pred_adj_context_',context,'.csv'))[time_id<0, .(location_id, time_id, q2.5, q97.5)]
  pi[, pi_width := q97.5 - q2.5]
  mean_pi_width <- mean(pi$pi_width)
  
  # make a row of the table
  temp_dt <- data.table(context_id = context,
                        emp_coverage = coverage$coverage_pre,
                        multiplier = coverage$multiplier,
                        mean_pi_width = mean_pi_width)
  return(temp_dt)
}

#performance_dt <- rbindlist(lapply(context_lookup$context_id, get_performance_stats))
performance_dt <- rbindlist(lapply(1:5, get_performance_stats))
performance_dt <- merge(context_lookup[,.(context_id, mandate_num, mandate_type, mandate_timing, pop_cat, pol_cat)],
                        performance_dt, by='context_id')

# Save table
fwrite(performance_dt, paste0(root_dir,'/model_performance_summary.csv'))


### PART 2 - SIGNAL DETECTION ###

# Compile results table
# Median effect size, IQR of the effect size, % of locations with effect in hypothesized direction
#TODO - add meta-analytic p-value/binomial p-value
calc_eff_size <- function(context){
  
  # load predictions
  pi <- fread(paste0(root_dir,'/batched_output/pred_adj_context_',context,'.csv'))[time_id==0, .(location_id, q2.5, q50, q97.5, p_val)]
  
  # load observed value
  obs <- fread(paste0(root_dir,'/batched_output/obs_context_',context,'.csv'))[time_id==0,.(location_id, y)]
  
  # combine
  dt <- merge(obs,pi, by='location_id')
  
  # calculate difference between observed and median if PI
  dt[, eff_size := y - q50]
  
  # calculate p-value across all locations
  k <- nrow(dt[p_val < 0.05]) #number of sig locs
  binom_p <- round(pbinom(k,nrow(dt),0.05,lower.tail=F),2)

  # calculate median and IQR of effect size across locations
  temp_dt <- data.table(context_id = context,
                        eff_size_median = round(median(dt$eff_size),2),
                        eff_size_Q1 = round(quantile(dt$eff_size, 0.25),2),
                        eff_size_Q3 = round(quantile(dt$eff_size, 0.75),2),
                        eff_size_IQR = round(IQR(dt$eff_size),2),
                        eff_pct_neg = round((sum(dt$eff_size<0)/nrow(dt))*100,1),
                        pct_sig_p = round((sum(dt$p_val<0.05)/nrow(dt))*100, 1),
                        binom_p = binom_p
                        )
  
  return(temp_dt)
}

#effect_size_dt <- rbindlist(lapply(context_lookup$context_id, calc_eff_size))
effect_size_dt <- rbindlist(lapply(1:5, calc_eff_size))
effect_size_dt <- merge(context_lookup[,.(context_id, mandate_num, mandate_type, mandate_timing, pop_cat, pol_cat)],
                        effect_size_dt, by='context_id')
# Save table
fwrite(effect_size_dt, paste0(root_dir,'/effect_size_summary.csv'))


# Make a forest plot

# Set levels of context_id wrt effect size so they appear in ascending
# order in the plot
effect_size_dt <- effect_size_dt %>%
  arrange(desc(eff_size_median)) %>%
  mutate(context_id = factor(context_id, levels = unique(context_id)))

# TODO sort by eff_size_median
my_colors <- c("D" = "blue", "M" = "purple", "R" = "red")
ggplot(effect_size_dt, aes(x=context_id, y=eff_size_median, ymin=eff_size_Q1, ymax=eff_size_Q3)) +
  geom_hline(yintercept=0, linetype='dashed') +
  geom_pointrange(aes(color=pol_cat, pch=pop_cat)) +
  scale_color_manual(name="Political Affiliation", values = my_colors,
                     labels = c("D"="Democrat", "M"="Moderate", "R"="Republican")) +
  scale_shape_manual(name="Population", values = c('big' = 16, 'small' = 1), labels = c("big" = "100K+", "small" = "<100K")) +
  scale_x_discrete(name="", labels=NULL) +
  scale_y_continuous(name="Log Ratio") +
  ggtitle('1st Restaurant Mandates') +
  coord_flip() +
  theme_classic() +
  theme(axis.ticks.y = element_blank())
