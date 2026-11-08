# Diagnostic Time Series Plots

### Prep
# libraries
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# args
version_id <- '20261013.02'

# directories
root_dir  <- '/ihme/scratch/users/ems2285/thesis/outputs/outputs/'
covar_dir <- '/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/USA_counties/'
config_dir  <- '/ihme/homes/ems2285/repos/simulate-pipeline/config_files/'
lookup_path <- paste0(config_dir, 'context_lookup_inv2_intersect_v3.csv')   # which contexts to analyze
locs_path   <- paste0(config_dir, 'locs_by_context_inv2_intersect_v3.csv')   # which counties are in each context

# location hierarchy info
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)
states_dt <- hierarchy[level==2, .(parent_id = location_id, state_abbrev = gsub('US-','',local_id))]
counties_dt <- hierarchy[level==3, .(parent_id, location_id, location_name)]
locs_dt <- merge(counties_dt, states_dt, by='parent_id')


#### Load data
# Context definitions
context_info <- fread(lookup_path)[, .(context_id, mandate_num, mandate_type, state_abbrev, N)]

# Observations
obs_dt <- fread(paste0(root_dir, version_id, '/batched_output/obs_context_', context, '.csv'))[
  !is.na(y), .(location_id, time_id, y)]

# Mandate timing
dining_dt <- fread(paste0(covar_dir, 'first_restaurant_close.csv'))
bar_dt <- fread(paste0(covar_dir, 'first_bar_close.csv'))
