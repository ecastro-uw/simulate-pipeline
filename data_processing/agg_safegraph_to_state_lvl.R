# Aggregate safegraph to state level

# Load packages
library(data.table)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# Directories
input_path <- "/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/USA_counties/processed_safegraph_data.csv"
output_path <- "/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/USA_states/processed_safegraph_data.csv"

## Prep hierarchy ---
# Load US counties hierarchy
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)
abbrevs_dt <- hierarchy[level==2][, .(location_id, location_name, state= substr(local_id, 4,5))]

# Load county-level data
county_dt <- fread(input_path)[, .(state, date, top_category, visit_count)]
state_dt <- county_dt[, sum(visit_count), by = c('state', 'date', 'top_category')]
setnames(state_dt, 'V1', 'visit_count')

# Add IHME location id
state_dt <- merge(state_dt, abbrevs_dt, by='state', all.x=T)
state_dt <- state_dt[, .(location_id, date, top_category, visit_count)]

# Order
state_dt <- state_dt[order(location_id, date, top_category)]

# Save
fwrite(state_dt, output_path)
