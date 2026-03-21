
# Process county-level data reflecting cumulative values of reported COVID-19 cases and deaths
# Source: New York Times, https://github.com/nytimes/covid-19-data

source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# Load location hierarchy
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)
counties <- hierarchy[level==3 & local_id!="", .(location_id, fips = as.integer(local_id))]
# Note the locations in the hierarchy without local_id/fips code
no_fips <- hierarchy[level==3 & local_id=="", .(parent_id,location_id,location_name,level)]
no_fips <- merge(no_fips, hierarchy[, .(parent_id = location_id, parent_name= location_name)],
                 by='parent_id', all.x=T)

# Load the raw data
dt <- fread('/ihme/scratch/users/ems2285/thesis/inputs/data/us-counties-2020_NYT.csv')

# Note the locations in the NYT database that dont map to the hierarchy
in_NYT_only <- unique(merge(dt, counties, by='fips', all.x=T)[is.na(location_id), .(county,state)])
in_NYT_only <- in_NYT_only[! state %in% c('Puerto Rico', 'Virgin Islands', 'Northern Mariana Islands','Guam')]
# Most states have at least some of their deaths attributed to "unknown" county. 
dt[county=="Unknown", sum(deaths), by=state]
# AK, AZ, CO, FL, HI, MD, MO, MT, NM, SD, VA have counties with reported values not found in the IHME hierarchy
# NY does as well, but for the reason noted below
merge(dt, in_NYT_only[county!="Unknown"], by=c("county","state"))[, sum(deaths), by=state]

# Note the locations in the hierarchy that dont have any data
in_hier_only <- merge(counties, unique(dt[,.(fips, state)]), by='fips', all.x=T)[is.na(state)]
# The IHME hierarchy has a separate id for each borough of NYC whereas NYT reports deaths for
# all of New York City combined


# Add IHME location id
dt <- merge(dt, counties, by='fips')
dt <- dt[, .(location_id, date, cases, deaths)]

# Make the dataset square
all_locs <- unique(dt$location_id)
all_dates <- seq(as.Date('2020-01-01'), as.Date('2020-12-31'), by="days")
template <- data.table(crossing(location_id=all_locs,
                                date=all_dates))
all_dt <- merge(template, dt, by=c('location_id','date'), all.x=T)
all_dt[is.na(all_dt)] <- 0

# Add daily values
all_dt[, c("daily_cases", "daily_deaths") :=
         list(cases - shift(cases, fill = 0), deaths - shift(deaths, fill=0)), by=location_id]

# Rename cumulative values
setnames(all_dt, c('cases','deaths'), c('cuml_cases', 'cuml_deaths'))
# Note that this results in some negative values

# Output
fwrite(all_dt, '/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/covid_cases_deaths.csv')
