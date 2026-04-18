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


library(scoringutils, lib.loc = '/ihme/homes/ems2285/lib_for_scoringutils')
library(data.table)
library(ggplot2)
library(dplyr)

# args
version_id <- '20260417.03'

# dirs
root_dir <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/outputs/',version_id)

# load context lookup file
context_lookup <- fread(file.path(root_dir,'inputs/context_lookup_table.csv'))

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

# Calculate forecast skill (skill = 1 - (wis_ens/wis_naive))
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
performance_dt <- rbindlist(lapply(context_lookup$context_id, build_performance_table))

# Add context info
performance_dt <- merge(context_lookup[,.(context_id, mandate_num, mandate_type, pop_cat, pol_cat, N)],
                        performance_dt, by='context_id')

# Save table
fwrite(performance_dt, paste0(root_dir,'/model_performance_summary.csv'))


# Compile other measures of model performance (not currently output)
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

#addn_performance_dt <- rbindlist(lapply(context_lookup$context_id, get_performance_stats))
#addn_performance_dt <- merge(context_lookup[,.(context_id, mandate_num, mandate_type, pop_cat, pol_cat, N)], 
#                             addn_performance_dt, by='context_id')




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

effect_size_dt <- rbindlist(lapply(context_lookup$context_id, calc_eff_size))
effect_size_dt <- merge(context_lookup[,.(context_id, mandate_num, mandate_type, pop_cat, pol_cat, N)],
                        effect_size_dt, by='context_id')
# Save table
fwrite(effect_size_dt, paste0(root_dir,'/effect_size_summary.csv'))


### (B) Make a forest plot

# Update the context ids within each event type so the intervals line up vertically
# in the final plot
plot_dt <- copy(effect_size_dt)
plot_dt[, context_new :=  (context_id - 1) %% 6 + 1 ]

my_colors <- c("D" = "blue", "M" = "#E69F00", "R" = "red")

p1 <- ggplot(plot_dt, aes(x = context_new, y = eff_size_median, ymin = eff_size_Q1, ymax = eff_size_Q3)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = "gray40") +
  geom_pointrange(
    aes(color = pol_cat, pch = pop_cat),
    size = 0.5
  ) +
  scale_color_manual(
    name = "Political Affiliation",
    values = my_colors,
    labels = c("D" = "Democrat", "M" = "Moderate", "R" = "Republican")
  ) +
  scale_shape_manual(
    name = "Population",
    values = c('big' = 16, 'small' = 2),  # triangle instead of open circle for small
    labels = c("big" = "100K+", "small" = "<100K")
  ) +
  scale_x_discrete(name = "", labels = NULL) +
  scale_y_continuous(name = "Log Ratio") +
  coord_flip() +
  facet_grid(mandate_num ~ mandate_type, labeller = labeller(
    mandate_num = c("first" = "First", "second" = "Second"),
    mandate_type = c("bar" = "Bar", "restaurant" = "Restaurant"))
  ) +
  theme_bw() +
  theme(
    axis.ticks.y = element_blank(),
    strip.text.y = element_text(angle = 0),          # readable strip labels
    strip.background = element_rect(fill = "gray95", color = "gray70"),
    panel.border = element_rect(color = "gray70"),   # lighter panel borders
    panel.grid.major.y = element_blank(),            # remove horizontal gridlines (they're meaningless without y labels)
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

pdf(paste0(root_dir,'/forest_plot.pdf'), width=8, height=5)
p1
dev.off()