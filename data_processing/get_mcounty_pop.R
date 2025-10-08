# Load location hierarchy
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
mcnty_loc_set <- get_location_metadata(location_set_id = 128, location_set_version_id = 1249, release_id = 16)


# Load USHD database
library(lbd.loader, lib.loc = sprintf("/share/geospatial/code/geospatial-libraries/lbd.loader-%s.%s", 
                                      R.version$major, 
                                      strsplit(R.version$minor, '.', fixed = TRUE)[[1]][[1]]))
library("ushd.dbr", lib.loc = lbd.loader::pkg_loc("ushd.dbr"))

# Pull population estimates for 2019 (unique by mcounty, age, and sex)
pop_dt <- get_population_data(population_name = "pop_by_age_sex", year=list(2019))

# Summarize to mcounty level
county_pop <- pop_dt[, sum(pop), by='mcnty']
setnames(county_pop, 'V1', 'pop')

# mapping of mcounty to location_id
loc_map <- fread('/snfs1/Project/us_counties/locations/counties/merged_counties.csv')




####
pop_versions <- get_covariate_version("pop_by_age_sex")

pop_dt <- get_population_data(covariate_dataset_id = 267,
                              year=list(2019))
                              #sex = list(3),
                              #age = list(98) 

pop_dt <- get_population_data(
  covariate_dataset_id = NULL,
  population_name = NULL,
  population_id = NULL,
  columns = NULL,
  mcnty = NULL,
  year = NULL,
  edu = NULL,
  sex = NULL,
  age = NULL,
  race = NULL
)


