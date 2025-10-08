# Prep county population data
# Source: https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/totals/

library(data.table)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

input_path <- "/ihme/scratch/users/ems2285/thesis/intermediate/visits_by_cnty_day_cat_v2.csv"
output_path <- "/ihme/scratch/users/ems2285/thesis/intermediate/visits_by_cnty_day_cat_v3.csv"

# load the location hierarchy
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)
abbrevs <- hierarchy[level==2, .(location_name, state=substr(local_id,4,5))]

# load data file
dt <- fread(input_path)

# make names compatible with pop file
dt[, county:=tolower(county)]
dt[, county:=gsub('st\\.', "saint", county)]

# load the census data file
pop_dt <- as.data.table(read.csv("/mnt/share/scratch/users/ems2285/thesis/data/census_county_est2023_alldata.csv", fileEncoding='latin1'))
pop_dt <- pop_dt[,.(state=STNAME, county=CTYNAME, pop=POPESTIMATE2020)]

# drop states from the dt
pop_dt <- pop_dt[state!=county]

# make names compatible with mobility file
pop_dt[, county:=tolower(county)]
pop_dt[, county:=gsub('st\\.', "saint", county)]
setnames(pop_dt, 'state', 'location_name')
pop_dt <- merge(pop_dt, abbrevs, by='location_name')

# add population data
dt <- merge(dt, pop_dt, by=c('county','state'), all.x=T)

# Calculate visits per 100K
dt[, visits_per_10k := (visit_count/pop)*10000]

fwrite(dt, output_path)



### county-level pop files
county_names <- fread("/mnt/share/dex/us_county/maps/merged_counties.csv")[ , cnty := sprintf("%05d", cnty)][, 1:5]

mcnty_pop <- fread("/mnt/share/dex/us_county/03_post_model/pop_denom/best/inputs/county_population_age_sex.csv")[ year_id >= 2010, ][ , pop := sum(pop, na.rm = T), by = c('year_id', 'mcnty')]