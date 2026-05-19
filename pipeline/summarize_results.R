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
library(tools)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# args
version_id <- '20260517.01'

# dirs
root_dir <- file.path('/ihme/scratch/users/ems2285/thesis/outputs/outputs',version_id)

# load context lookup file
context_lookup <- fread(file.path(root_dir,'inputs/context_lookup_table.csv'))

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


### (B) Meta-regression
fit <- lm(eff_size_median ~ mandate_type + mandate_num + pop_cat + pol_cat, data=effect_size_dt)
summary(fit)
ggplot(effect_size_dt, aes(x=pol_cat, y=eff_size_median, color=mandate_type, shape=mandate_num)) +
  geom_point(size=2) +
  theme_classic()

### (C) Make a forest plot

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
  scale_y_continuous(name = "Effect Size") +
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


### PART 2 - VETTING PLOTS ###

### (A) Check that all contexts successfully optimized ensemble weights
fit_files <- list.files(file.path(root_dir, 'batched_output'), pattern = 'ens_fit_stats', full.names=T)
dt_all <- rbindlist(lapply(fit_files, function(x) {
  dt <- fread(x)
  dt[, context_id := as.integer(gsub('.*_context_(\\d+)\\.csv$', '\\1', basename(x)))]
  dt
}))

# Print contexts with non-convergence
dt_all[convergence!=0]

### (B) Ensemble weight heat map

# Read and combine all 24 files
all_weights <- rbindlist(
  lapply(1:24, function(x) {
    dt <- fread(paste0(root_dir,'/batched_output/ens_weights_context_',x,'.csv'))
    dt[, context := x]
    dt
  }),
  fill = TRUE
)

# Pivot wide, filling missing model-context combos with 0
wide <- dcast(all_weights, context ~ model, value.var = "weight", fill = 0)

# Melt back to long for ggplot
long <- melt(wide, id.vars = "context", variable.name = "model", value.name = "weight")

# Order models numerically (model_1, model_2, ..., model_20)
long[, model_num := factor(gsub('model_', '',model), levels = 1:30)]
#long[, model := factor(model, levels = paste0("model_", 1:30))]

# rank models
long[, rank := frankv(weight, order = -1L), by = context]


# Plot
p2 <- ggplot(long, aes(x = model_num, y = factor(context), fill = weight)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(data = long[rank <= 3],
            aes(label=rank), size=3, fontface="bold") +
  scale_fill_gradientn(
    colors = c("white", "#fee8c8", "#e34a33"),
    #trans = "log1p",
    name = "Weight",
    labels = scales::scientific
  ) +
  scale_y_discrete(limits = rev) +          # context 1 at top
  labs(
    title = "Ensemble Weights by Context and Model",
    x = "Model",
    y = "Context"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    panel.grid   = element_blank()
  )

pdf(paste0(root_dir,'/ens_weight_heat_map.pdf'), width=8, height=5)
p2
dev.off()


### (C) Time series plots by location
plot_timeseries <- function(context, model_name='ensemble', save_pdf = FALSE){
  obs_dt <- fread(paste0(root_dir,'/batched_output/obs_context_',context,'.csv'))[, .(location_id, time_id, y)]
  if (model_name == 'ensemble') {
    pred_dt <- fread(paste0(root_dir,'/batched_output/pred_adj_context_',context,'.csv'))
  } else {
    pred_dt <- fread(paste0(root_dir,'/batched_output/candidate_mods_context_',context,'.csv'))[model==model_name,
                     .(location_id, time_id, q50, q2.5, q97.5)]
  }
  # combine observed time series and prediction intervals
  plot_dt <- merge(obs_dt, pred_dt[,.(location_id, time_id, median = q50, LL = q2.5, UL = q97.5)], 
                   by=c('location_id', 'time_id'), all.x=T)
  
  # add location names
  plot_dt <- merge(plot_dt, counties[,.(location_id, full_name, state)], by='location_id', all.x=T)
  
  # define the plot
  make_plot <- function(dt) {
    ggplot(dt) +
      geom_point(aes(x=time_id, y=y)) +
      geom_line(aes(x=time_id, y=y)) +
      geom_errorbar(aes(x=time_id, y=median, ymin=LL, ymax=UL), width=0.4, color='red') +
      geom_point(data=dt[!is.na(median)], aes(x=time_id, y=median), color='red', pch=2) + 
      #geom_errorbar(data=plot_dt[time_id==0], aes(x=time_id, y=mean, ymin=LL, ymax=UL), width=0.4, color='red') +
      theme_classic() +
      facet_wrap(~full_name)
  }
  
  # build a list of plots
  plots <- if (length(unique(plot_dt$full_name)) <= 10) {
    # all plots on same page
    list(make_plot(plot_dt))
  } else {
    # one plot per state
    lapply(unique(plot_dt$state), function(st) make_plot(plot_dt[state == st]))
  }
  
  # Either save to PDF or print to console
  if (save_pdf) {
    path <- paste0(root_dir,'/timeseries_obs_vs_',model_name,'_context_',context,'.pdf')
    pdf(path)
    lapply(plots, print)
    dev.off()
    message("Saved to", path)
  } else {
    lapply(plots, print)
  }
  invisible(plots)
}


# Does the ensemble fit well for first restaurant impositions?
# In instances with early outbreaks: yes because cases are a decent
# predictor. Otherwise, not really as it tends to miss the pre-emptive drop off
# in mobility occuring 1-2 weeks ahead of the mandate

# Could try stratifying counties based upon case load/onset of outbreak?

# big dem
plot_timeseries(context = 1,
                model_name = "ensemble",
                save_pdf = F)
# big mod
plot_timeseries(context = 4,
                model_name = "ensemble",
                save_pdf = F)


### (D) Compare IS/OOS and Unadj/Adj intervals
compare_pred_intervals <- function(context, save_pdf = FALSE){
  
  # Resolve context
  context_lookup <- fread(paste0(root_dir, '/inputs/context_lookup_table.csv'))
  one_row <- context_lookup[context_id==context]
  context_name <- toTitleCase(paste0(one_row$mandate_num, ' ', one_row$mandate_type, ' mandates among ',
                        one_row$pop_cat, ' ', ifelse(one_row$pol_cat=='D', 'Democratic', 
                        ifelse(one_row$pol_cat=='M', 'Moderate', 'Republican')), ' counties (context ', context,')'))
  
  # Define columns to keep from each file
  col_names <- c('location_id', 'time_id', 'q2.5', 'mean', 'q97.5')
  
  # load the data and predictions
  obs_dt <- fread(paste0(root_dir,'/batched_output/obs_context_',context,'.csv'))[time_id > -2 & time_id < 0, .(location_id, time_id, y)]
  unadj_is <- fread(paste0(root_dir,'/batched_output/pred_pre_context_',context,'.csv'))[time_id==-1, .SD, .SDcols=col_names]
  unadj_oos <- fread(paste0(root_dir,'/batched_output/pred_pre_v2_context_',context,'.csv'))[time_id==-1, .SD, .SDcols=col_names]
  adj_is <- fread(paste0(root_dir,'/batched_output/pred_adj_context_',context,'.csv'))[time_id==-1, .SD, .SDcols=col_names]
  adj_oos <- fread(paste0(root_dir,'/batched_output/pred_adj_M2_context_',context,'.csv'))[time_id==-1, .SD, .SDcols=col_names]
  multipliers <- fread(paste0(root_dir,'/batched_output/coverage_context_',context,'.csv'))
  
  # combine
  unadj_is[, `:=` (type = 'IS', adj = 'No', cats = 'IS unadj')]
  unadj_oos[, `:=` (type = 'OOS', adj = 'No', cats = 'OOS unadj')]
  adj_is[, `:=` (type = 'IS', adj = 'Yes', cats = 'IS adj')]
  adj_oos[, `:=` (type = 'OOS', adj = 'Yes', cats = 'OOS adj')]
  plot_dt <- rbind(unadj_is, unadj_oos)
  plot_dt <- rbind(plot_dt, adj_is)
  plot_dt <- rbind(plot_dt, adj_oos)
  
  # enumerate locations
  loc_ids <- unique(plot_dt$location_id)
  
  # Define category sort order and colors
  cats_levels <- c("IS unadj", "IS adj", "OOS unadj", "OOS adj")
  plot_dt[, cats := factor(cats, levels = cats_levels)]
  cats_colors <- c(
    "IS unadj"  = "#2166AC",  # blue
    "IS adj"    = "#762A83",  # purple
    "OOS unadj" = "#B2182B",  # red
    "OOS adj"   = "#D4760A"   # orange
  )
  
  # Define coverage table for each category
  coverage_dt <- merge(plot_dt, obs_dt[time_id==-1], by=c('location_id', 'time_id'))
  coverage_dt[, in_interval := ifelse(y >= q2.5 & y <= q97.5, 1, 0)]
  summary_dt <- coverage_dt[ , sum(in_interval)/length(loc_ids), by=cats]
  setnames(summary_dt, 'V1', 'coverage')
  summary_dt <- summary_dt[order(cats)]
  summary_dt$multiplier <- c('NA', round(multipliers$multiplier,2),
                             'NA', round(multipliers$multiplier2,2))
  
  # add location names
  plot_dt <- merge(plot_dt, counties[,.(location_id, full_name, state)], by='location_id', all.x=T)
  
  # define jitter 
  offsets <- seq(-0.2, 0.2, length.out = 4)
  offset_map <- setNames(offsets, levels(plot_dt$cats))
  plot_dt[, x_jitter := time_id + offset_map[as.character(cats)]]
  
  # Build individual plots
  plot_one_loc <- function(loc_id){
    
  p <- ggplot() +
    # Observed points
    geom_point(
      data = obs_dt[location_id == loc_id],
      aes(x = time_id, y = y),
      shape = 21, fill = "white", color = "black",
      size = 3, stroke = 1.2
    ) +
    # Prediction interval error bars (jittered)
    geom_errorbar(
      data = plot_dt[location_id == loc_id],
      aes(x = x_jitter, y = mean, ymin = q2.5, ymax = q97.5, color = cats),
      width = 0.1, linewidth = 0.8, alpha = 0.85
    ) +
    # Mean point per model
    geom_point(
      data = plot_dt[location_id == loc_id],
      aes(x = x_jitter, y = mean, color = cats),
      size = 2
    ) +
    scale_x_continuous(
      name = "",
      breaks = c(-3, -2, -1)
    ) +
    scale_y_continuous(name = "") +
    scale_color_manual(values = cats_colors, breaks = cats_levels, name = "") +
    #ggtitle(plot_dt[location_id == loc_id, unique(full_name)]) +
    theme_classic() +
    theme(
      legend.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
    
    # Strip legend (one legend will be added back)
    p + theme(legend.position = "none")
    #return(p)
  }
  
  # make plots for all locations
  plots <- lapply(loc_ids, plot_one_loc)
  
  # Extract legend from a single full plot
  legend_plot <- ggplot() +
    geom_point(
      data = plot_dt[location_id == loc_ids[1]],
      aes(x = x_jitter, y = mean, color = cats),
      size = 2
    ) +
    scale_color_brewer(palette = "Set1", name = "") +
    theme_classic() +
    theme(
      legend.text = element_text(size = 10),
      legend.position = "bottom"
    )
  
  shared_legend <- cowplot::get_legend(legend_plot)
  
  # Combine plots + legend with patchwork
  grid <- patchwork::wrap_plots(plots) +
    patchwork::plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  # Add a title
  grid <- grid + patchwork::plot_annotation(title = context_name)
  
  # Either save to PDF or print to console
  if (save_pdf) {
    path <- paste0(root_dir,'/compare_PIs_context_',context,'.pdf')
    pdf(path, width = 11, height=8)
    print(grid)
    dev.off()
    message("Saved to", path)
  } else {
    print(grid)
  }
  invisible(grid)
  return(summary_dt)
}

compare_pred_intervals(7, save_pdf = T)




just_the_table <- function(context){
  
  # Resolve context
  context_lookup <- fread(paste0(root_dir, '/inputs/context_lookup_table.csv'))
  one_row <- context_lookup[context_id==context]
  context_name <- toTitleCase(paste0(one_row$mandate_num, ' ', one_row$mandate_type, ' mandates among ',
                                     one_row$pop_cat, ' ', ifelse(one_row$pol_cat=='D', 'Democratic', 
                                                                  ifelse(one_row$pol_cat=='M', 'Moderate', 'Republican')), ' counties (context ', context,')'))
  
  # Define columns to keep from each file
  col_names <- c('location_id', 'time_id', 'q2.5', 'mean', 'q97.5')
  
  # load the data and predictions
  obs_dt <- fread(paste0(root_dir,'/batched_output/obs_context_',context,'.csv'))[time_id > -2 & time_id < 0, .(location_id, time_id, y)]
  unadj_is <- fread(paste0(root_dir,'/batched_output/pred_pre_context_',context,'.csv'))[time_id==-1, .SD, .SDcols=col_names]
  unadj_oos <- fread(paste0(root_dir,'/batched_output/pred_pre_v2_context_',context,'.csv'))[time_id==-1, .SD, .SDcols=col_names]
  adj_is <- fread(paste0(root_dir,'/batched_output/pred_adj_context_',context,'.csv'))[time_id==-1, .SD, .SDcols=col_names]
  adj_oos <- fread(paste0(root_dir,'/batched_output/pred_adj_M2_context_',context,'.csv'))[time_id==-1, .SD, .SDcols=col_names]
  multipliers <- fread(paste0(root_dir,'/batched_output/coverage_context_',context,'.csv'))
  
  # combine
  unadj_is[, `:=` (type = 'IS', adj = 'No', cats = 'IS unadj')]
  unadj_oos[, `:=` (type = 'OOS', adj = 'No', cats = 'OOS unadj')]
  adj_is[, `:=` (type = 'IS', adj = 'Yes', cats = 'IS adj')]
  adj_oos[, `:=` (type = 'OOS', adj = 'Yes', cats = 'OOS adj')]
  plot_dt <- rbind(unadj_is, unadj_oos)
  plot_dt <- rbind(plot_dt, adj_is)
  plot_dt <- rbind(plot_dt, adj_oos)
  
  # enumerate locations
  loc_ids <- unique(plot_dt$location_id)
  
  # Define category sort order and colors
  cats_levels <- c("IS unadj", "IS adj", "OOS unadj", "OOS adj")
  plot_dt[, cats := factor(cats, levels = cats_levels)]
  
  # Define coverage table for each category
  coverage_dt <- merge(plot_dt, obs_dt[time_id==-1], by=c('location_id', 'time_id'))
  coverage_dt[, in_interval := ifelse(y >= q2.5 & y <= q97.5, 1, 0)]
  summary_dt <- coverage_dt[ , sum(in_interval)/length(loc_ids), by=cats]
  setnames(summary_dt, 'V1', 'coverage')
  summary_dt <- summary_dt[order(cats)]
  summary_dt$multiplier <- c('NA', round(multipliers$multiplier,2),
                             'NA', round(multipliers$multiplier2,2))
  summary_dt[, context_id := context]
  return(summary_dt)
}

all_contexts <- rbindlist(lapply(c(1:11,13:24), just_the_table))
fwrite(all_contexts, paste0(root_dir,'/coverage_compare.csv'))
