
## SET UP
# libraries
library(data.table)
library(patchwork)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# args
Scenario <- 'C'

# dirs
versions_path <- '/ihme/scratch/users/ems2285/thesis/inputs/sensitivity_version_ids.csv'
root_dir <- '/ihme/scratch/users/ems2285/thesis/outputs/outputs/'

## LOADING
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)
states_dt <- hierarchy[level==2, .(location_id, state_abbrev = gsub('US-','',local_id))]
versions_dt <- fread(versions_path, colClasses = 'character')[scenario==Scenario]
list_of_cols <- c('context_id', 'mandate_num', 'mandate_type', 'state_abbrev', 'N')

# define a function for processing one week (both tests)
combine_results <- function(row){
  # resolve versions
  week <- versions_dt[row, week]
  version <- versions_dt[row, version]
  input_path <- file.path(root_dir, version)

  
  # load results from test 1
  p_val <- fread(paste0(input_path,'/effect_size_summary.csv'))[, .SD, .SDcols = c(list_of_cols, 'binom_p')]
  p_val[, (week) := ifelse(binom_p < 0.05, 1, 0)]
  p_val$binom_p <- NULL
  
  # load results from test 2
  rr <- fread(paste0(input_path,'/effect_size_meta.csv'))[, .SD, .SDcols = c(list_of_cols, 'direction')]
  rr[, (week) := ifelse(direction=='decrease', 1, 0)]
  rr$direction <- NULL
  
  list(p_val = p_val, rr = rr)
}

# Build one wide table per test, one column per week
test1_dt <- NULL
test2_dt <- NULL

for (row in seq_len(nrow(versions_dt))) {
  result <- combine_results(row)
  if (is.null(test1_dt)) {
    test1_dt <- result$p_val
    test2_dt <- result$rr
  } else {
    test1_dt <- merge(test1_dt, result$p_val, by = list_of_cols, all = TRUE)
    test2_dt <- merge(test2_dt, result$rr, by = list_of_cols, all = TRUE)
  }
}

# For each test, identify contexts where only week 0 = 1 and all other weeks = 0
week_cols <- setdiff(names(test1_dt), list_of_cols)
week0_col <- "0"

test1_dt[, test1 := as.integer(get(week0_col) == 1 & rowSums(.SD) == 1), .SDcols = week_cols]
test2_dt[, test2 := as.integer(get(week0_col) == 1 & rowSums(.SD) == 1), .SDcols = week_cols]

# For each test, identify contexts where ALL weeks = 0
test1_dt[, test1_YYYYY := as.integer(rowSums(.SD)==0), .SDcols = week_cols]
test2_dt[, test2_YYYYY := as.integer(rowSums(.SD)==0), .SDcols = week_cols]

# Combine results from the two tests
results_dt <- merge(test1_dt[, .SD, .SDcols = c(list_of_cols, 'test1', 'test1_YYYYY')],
                    test2_dt[, .SD, .SDcols = c(list_of_cols, 'test2', 'test2_YYYYY')])

# Get appropriate version id
week0_vers <- versions_dt[week==week0_col, version]


###  1. Investigate contexts that passed Test 1 (i.e. "A+C")

# Define one plot
plot_one_state <- function(context, covariate){
  
  # context info
  context_info <- fread(paste0(root_dir, week0_vers, '/inputs/context_lookup_table.csv'))[context_id==context]
  mandate_type <- context_info$mandate_type
  mandate_num <- context_info$mandate_num
  
  # y axis: p<0.05?
  dt <- fread(paste0(root_dir, week0_vers,'/batched_output/pred_adj_context_',context,'.csv'))[time_id==0]
  dt[, y_val := ifelse(p_val < 0.05, 'yes', 'no')]
  counties <- unique(dt$location_id)
  
  # x axis: covariate of choice
  covar_dir <- "/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/USA_counties/"
  # (A) county population
  if(covariate=='population'){
    covar_dt <- fread(paste0(covar_dir,"population.csv"))[location_id %in% counties]
    setnames(covar_dt, 'pop', 'x_val')
    x_lab <- "Population"
  }
  
  # (B) baseline visits (median restaurant/bar visits per 10K pop during 2019)
  if(covariate=='visits'){
    covar_dt <- fread(paste0(covar_dir,"/processed_safegraph_data.csv"))[location_id %in% counties]
    if(mandate_type=='restaurant'){
      covar_dt <- covar_dt[top_category %like% 'Restaurant']
    } else{ #mandate_type=='bar'
      covar_dt <- covar_dt[! top_category %like% 'Restaurant']
    }
    covar_dt <- covar_dt[date>='2019-01-01' & date<='2019-12-31', .(location_id, date, state, visits_per_10k)]
    covar_dt <- covar_dt[, .(x_val = round(median(visits_per_10k),1)), by=c('location_id', 'state')]
    x_lab <- paste(mandate_type, covariate,'per 10K')
  }
  
  # (C) covid cases (cumulative cases on date of manate implementation)
  if(covariate=='cases'){
    
    # load mandate onset date and population
    mandate_dt <- fread(paste0(covar_dir,"/",mandate_num,"_",mandate_type,"_close.csv"))[location_id %in% counties]
    pop_dt <- fread(paste0(covar_dir,"population.csv"))[location_id %in% counties]
    mandate_dt <- merge(mandate_dt[, .(location_id, onset_date)],
                        pop_dt[, .(location_id, state, pop)], by='location_id')
    
    # load case and death data
    covar_dt <- fread(paste0(covar_dir,"/covid_cases_deaths.csv"))[location_id %in% counties]
    covar_dt <- merge(covar_dt[,.(location_id,date,cuml_cases)],
                      mandate_dt, by='location_id')
    
    # calc cumulative cases per 10K at time of mandate onset
    covar_dt <- covar_dt[date==onset_date][, .(location_id, state, x_val = (cuml_cases/pop)*10000)]
    x_lab <- 'cumulative cases per 10K'
  }
  
  # (D) political affiliation (% voted for trump)
  if(covariate=='political'){
    covar_dt <- fread(paste0(covar_dir,"/election_results.csv"))[location_id %in% counties & party=='REPUBLICAN']
    covar_dt <- covar_dt[, .(location_id, x_val = votes / total_votes)]
    state <- unique(fread(paste0(covar_dir,"population.csv"))[location_id %in% counties]$state)
    covar_dt$state <- state
    x_lab <- '% voted for Trump'
  }
  
  # combine
  dt <- merge(dt, covar_dt, by='location_id', all.x=T)
  
  # plot
  plot <- ggplot(dt, aes(x=x_val, y=y_val)) +
    geom_point() +
    scale_x_continuous(name=x_lab) +
    scale_y_discrete(name="p<0.05") +
    theme_bw() +
    ggtitle(unique(dt$state))

  return(plot)
}

# Define grid of plots
make_state_panel_plot <- function(results_dt, mandate_type_arg, mandate_num_arg, covariate) {
  
  test_1_pass <- results_dt[mandate_type == mandate_type_arg & mandate_num == mandate_num_arg & test1 == 1]
  
  plot_list <- lapply(seq_len(nrow(test_1_pass)), function(i) {
    plot_one_state(
      context   = test_1_pass[i, context_id],
      covariate = covariate
    )
  })
  
  combined_plot <- wrap_plots(plot_list, ncol = ceiling(sqrt(length(plot_list)))) +
    plot_annotation(title = paste(mandate_num_arg, mandate_type_arg, "mandate"))
  
  combined_plot + plot_layout(axis_titles = "collect")
}

## 1(a) - 2nd restaurant mandates
make_state_panel_plot(
  results_dt = results_dt, mandate_type_arg = "restaurant", mandate_num_arg = "second",
  covariate = "population")
make_state_panel_plot(
  results_dt = results_dt, mandate_type_arg = "restaurant", mandate_num_arg = "second",
  covariate = "visits")
make_state_panel_plot(
  results_dt = results_dt, mandate_type_arg = "restaurant", mandate_num_arg = "second",
  covariate = "cases")
make_state_panel_plot(
  results_dt = results_dt, mandate_type_arg = "restaurant", mandate_num_arg = "second",
  covariate = "political")

## 1(b) - 1st bar mandates
make_state_panel_plot(
  results_dt = results_dt, mandate_type_arg = "bar", mandate_num_arg = "second",
  covariate = "population")
make_state_panel_plot(
  results_dt = results_dt, mandate_type_arg = "bar", mandate_num_arg = "second",
  covariate = "visits")
make_state_panel_plot(
  results_dt = results_dt, mandate_type_arg = "bar", mandate_num_arg = "first",
  covariate = "cases")
make_state_panel_plot(
  results_dt = results_dt, mandate_type_arg = "bar", mandate_num_arg = "first",
  covariate = "political")


###  2. Investigate contexts that passed Test 2 (i.e. "B+C")

plot_test2 <- function(results_dt, mandate_type_arg, mandate_num_arg="first", covariate){
  
  # locs to plot
  locs_to_plot <- results_dt[mandate_type == mandate_type_arg & mandate_num == mandate_num_arg & (test2 == 1 | test2_YYYYY == 1)]
  locs_to_plot$test2 <- factor(locs_to_plot$test2,
                               levels = c(0, 1), 
                               labels = c("YYYYY", "YYGYY"))
  # add location id for each state/context
  locs_to_plot <- merge(locs_to_plot[, .(context_id,state_abbrev, test2, test2_YYYYY)],
                        states_dt, by='state_abbrev', sort=FALSE)
  states <- locs_to_plot$location_id
  
  # for each state, retrieve the RR (point estimate and interval)
  rr <- fread(paste0(root_dir,week0_vers,'/effect_size_meta.csv'))[context_id %in% locs_to_plot$context_id,
                                                                   .(context_id, RR_median,RR_lower,RR_upper)]
  
  # For each state, retrieve the covariate of choice
  covar_dir <- "/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/USA_states/"
  
  # (A) Population density
  if(covariate=='pop_dens'){
    #TODO
  }
  
  # (B) % of the state who voted for Trump in 2020
  if(covariate=='political'){
    covar_dt <- fread(paste0(covar_dir,"/election_results.csv"))[location_id %in% states & party=='REPUBLICAN']
    covar_dt <- covar_dt[, .(location_id, x_val = votes / total_votes)]
    x_lab <- '% voted for Trump (2020)'
  }
  
  # (C) Cumulative cases per 10K at time of mandate imposition
  if(covariate=='cases'){
    #TODO
  }
  
  # (D) Baseline visits (median restaurant/bar visits per 10K pop during 2019)
  if(covariate=='visits'){
    covar_dt <- fread(paste0(covar_dir,"/processed_safegraph_data.csv"))[location_id %in% states]
    if(mandate_type_arg=='restaurant'){
      covar_dt <- covar_dt[top_category %like% 'Restaurant']
    } else{ #mandate_type_arg=='bar'
      covar_dt <- covar_dt[! top_category %like% 'Restaurant']
    }
    covar_dt <- covar_dt[date>='2019-01-01' & date<='2019-12-31', .(location_id, date, visit_count)]
    
    # add population
    pop_dt <- fread(paste0(covar_dir,"/population.csv"))[location_id %in% states]
    covar_dt <- merge(covar_dt, pop_dt, by='location_id')
    covar_dt <- covar_dt[, .(x_val = round(median((visit_count/pop)*10000),1)), by='location_id']
    x_lab <- paste('median',mandate_type_arg, covariate,'per 10K (2019)')
  }
  
  # combine
  plot_dt <- merge(locs_to_plot, rr, by='context_id')
  plot_dt <- merge(plot_dt, covar_dt, by='location_id', sort=F)
  
  # make the plot
  ggplot(plot_dt, aes(x=x_val, y=RR_median, color=test2)) +
    geom_hline(yintercept = 1, linetype='dashed') +
    geom_point(size=2) +
    geom_errorbar(aes(ymin=RR_lower, ymax=RR_upper), linewidth=1, alpha=0.7) +
    scale_x_continuous(name=x_lab) +
    scale_y_continuous(name='RR', limits=c(0,NA)) +
    scale_color_discrete(name='State-level verdict') +
    theme_bw() +
    ggtitle(paste(mandate_num_arg, mandate_type_arg, 'mandates: Test 2 verdict vs.', covariate))
}

plot_test2(results_dt, mandate_type_arg = "restaurant", covariate="political")
plot_test2(results_dt, mandate_type_arg = "restaurant", covariate="visits")
