# Create Location Groups
# group_id, description, locs

library(data.table)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

out_dir <- "/ihme/scratch/users/ems2285/thesis/aim_3/"

# Load the location hierarchy
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)


# Group 1: urban WA counties
wa_counties <- hierarchy[parent_id==570, .(location_id, location_name)]
urban_wa_counties <- wa_counties[location_name %in% 
                                   c('King County', 'Pierce County', 'Snohomish County',
                                     'Kitsap County', 'Thurston County', 'Clark County',
                                     'Skagit County', 'Whatcom County', 'Yakima County')]

# Group 2: rural WA counties
rural_wa_counties <- wa_counties[location_name %in%
                                   c('Garfield County','Ferry County', 'Island County', 'Okanogan County',
                                     'Pend Oreille County', 'San Juan County', 'Stevens County')]

# Group 3: all WA counties

# Group 4: all GA counties
ga_counties <- hierarchy[parent_id==hierarchy[location_name=='Georgia', location_id], .(location_id, location_name)]


# Compile the dataset
groups <- data.table(group_id = 1:4,
                     description = c('Urban WA counties', 'Rural WA counties', 'WA counties', 'GA counties'),
                     locations = c(paste(urban_wa_counties$location_id, collapse=','),
                                   paste(rural_wa_counties$location_id, collapse=','),
                                   paste(wa_counties$location_id, collapse=','),
                                   paste(ga_counties$location_id, collapse=',')))

fwrite(groups, paste0(out_dir,'location_groups.csv'))


