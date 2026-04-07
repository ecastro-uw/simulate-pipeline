# Prep county population data
# Source 1: https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/totals/
# Source 2: USHD team estimates

# load libs
library(data.table)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# define dirs
input_path <- "/ihme/scratch/users/ems2285/thesis/inputs/data/census_county_est2023_alldata.csv"
out_path <- '/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/USA_counties/population.csv'

# load the location hierarchy
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)
counties <- merge(hierarchy[level==3, .(location_id, county = location_name, parent_id)],
                  hierarchy[level==2,.(parent_id = location_id, state = location_name)], by='parent_id')
counties$parent_id <- NULL
counties[, county:=tolower(county)]
counties[, state:=tolower(state)]

# load the census data file
pop_dt <- as.data.table(read.csv(input_path, fileEncoding='latin1'))[,.(state=STNAME, county=CTYNAME, pop=POPESTIMATE2020)]
pop_dt[, county:=tolower(county)]
pop_dt[, state:=tolower(state)]

# split out states from counties
state_pop <- unique(pop_dt[state==county])
cnty_pop <- pop_dt[state!=county]


# Address some differences in naming convention
cnty_pop[, county:=gsub('st\\.', "saint", county)]
cnty_pop[, county:=gsub('ste\\.', "sainte", county)]
cnty_pop[county=="doña ana county", county := "dona ana county"]
counties[location_id==94072, county := 'bedford county']
counties[location_id==94078, county := 'alleghany county']
counties[location_id==94079, county := 'halifax county']
counties[location_id==94080, county := 'park county']
counties[location_id==94081, county := 'miami-dade county']
counties[location_id==94082, county := 'jackson county']
counties[location_id==94084, county := 'oglala lakota county']

# Merge ihme location ids
test <- merge(cnty_pop, counties, by=c('state','county'))

# Check for missing locs
missing_locs <- counties[location_id %in% setdiff(counties$location_id, test$location_id)]

# Output
county_pop_file <- test[, .(location_id, state, county, pop)]
fwrite(county_pop_file, paste0(out_path))
