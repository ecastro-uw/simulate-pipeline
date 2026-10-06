# Create vetting tables displaying results for cutpoints timed 
# one and two weeks before and after true mandate implementation 
# (-2, -1, 0, +1, +2)

# Measure 1: binomial p-value < 0.05?
# Measure 2: 95% CI of RR includes 1?

# Libraries
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# Versions
Scenario <- 'D'
versions_path <- '/ihme/scratch/users/ems2285/thesis/inputs/sensitivity_version_ids.csv'
versions_dt <- fread(versions_path, colClasses = 'character')[scenario==Scenario]

# Convert a week value from versions_dt (e.g. "-2", "0", "1", "+1") to the
# column label used in the vetting tables ("-2", "-1", "0", "+1", "+2")
format_wk_shift <- function(week){
  wk <- as.integer(week)
  if(is.na(wk)) stop(paste0("Invalid week value in versions_dt: '", week, "'"))
  ifelse(wk > 0, paste0('+', wk), as.character(wk))
}


### Prep location hierarchy -------
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)
st_abbrev_map <- hierarchy[level==2, .(location_name, state_abbrev = gsub('US-','',local_id))]

### (A) Measure 1 -----------------
load_pvals <- function(version_id, week){
  
  # root_dir
  root_dir <- paste0('/ihme/scratch/users/ems2285/thesis/outputs/outputs/',version_id)
  
  # load the data
  dt <- fread(paste0(root_dir,'/effect_size_summary.csv'))[, .(mandate_num, mandate_type, state_abbrev, binom_p)]
  
  # resolve the week shift from versions_dt
  wk_shift <- format_wk_shift(week)
  
  # rename the p-val column to reflect week shift
  setnames(dt, 'binom_p', wk_shift)
  return(dt)
}

# Extract and combine Measure 1 for all contexts and cutpoints
dt_list <- Map(load_pvals, versions_dt$version, versions_dt$week)
merged_dt <- Reduce(function(x, y) merge(x, y, by = c("mandate_num", "mandate_type", "state_abbrev"), all = TRUE), dt_list)

# Sort as desired
merged_dt <- merge(merged_dt, st_abbrev_map, by='state_abbrev', all.x=T)
merged_dt[, mandate_type := factor(mandate_type, levels = c('restaurant', 'bar'))]
merged_dt <- merged_dt[order(mandate_num, mandate_type, location_name)]
merged_dt <- merged_dt[, .(mandate_num, mandate_type, state_abbrev, `-2`, `-1`, `0`, `+1`, `+2`)]

# Save
fwrite(merged_dt, '/ihme/scratch/users/ems2285/thesis/outputs/shift_ITS_experiment_binom_p_exp_A.csv')


### (B) Measure 2 -----------------
load_thetas <- function(version_id, week){
  
  # root_dir
  root_dir <- paste0('/ihme/scratch/users/ems2285/thesis/outputs/outputs/',version_id)
  
  # load the data
  dt <- fread(paste0(root_dir,'/effect_size_meta.csv'))[, .(mandate_num, mandate_type, state_abbrev, direction)]
  dt[, direction := ifelse(direction=="decrease", "-1", ifelse(direction=="indeterminate", "0", "1"))]
  
  # resolve the week shift from versions_dt
  wk_shift <- format_wk_shift(week)
  
  # rename the p-val column to reflect week shift
  setnames(dt, 'direction', wk_shift)
  return(dt)
}

# Extract and combine Measure 2 for all contexts and cutpoints
theta_list <- Map(load_thetas, versions_dt$version, versions_dt$week)
merged_thetas <- Reduce(function(x, y) merge(x, y, by = c("mandate_num", "mandate_type", "state_abbrev"), all = TRUE), theta_list)

# Sort as desired
merged_thetas <- merge(merged_thetas, st_abbrev_map, by='state_abbrev', all.x=T)
merged_thetas[, mandate_type := factor(mandate_type, levels = c('restaurant', 'bar'))]
merged_thetas <- merged_thetas[order(mandate_num, mandate_type, location_name)]
merged_thetas <- merged_thetas[, .(mandate_num, mandate_type, state_abbrev, `-2`, `-1`, `0`, `+1`, `+2`)]

# Save
fwrite(merged_thetas, '/ihme/scratch/users/ems2285/thesis/outputs/shift_ITS_experiment_meta_theta_exp_A.csv')




ggplot(plot_dt) +
  geom_point(aes(x=time_id, y=y)) +
  geom_line(aes(x=time_id, y=y)) +
  geom_point(data=plot_dt[time_id==0], aes(x=time_id, y=median), color='red', pch=2) + 
  geom_errorbar(data=plot_dt[time_id==0], aes(x=time_id, y=median, ymin=LL, ymax=UL), width=0.4, color='red') +
  theme_classic()
