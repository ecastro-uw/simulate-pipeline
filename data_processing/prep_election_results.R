# Prep election data
# Source: https://electionlab.mit.edu/data 

# Load libs
library(data.table)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# Define dirs
input_path <- "/ihme/scratch/users/ems2285/thesis/inputs/data/countypres_2000-2024_MIT.csv"
out_path <- '/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/USA_counties/election_results.csv'

# load the location hierarchy
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)

# Load the raw data
dt <- fread(input_path)[year==2020 & mode=='TOTAL' & party %in% c('REPUBLICAN', 'DEMOCRAT'),
                        .(state = tolower(state), county = tolower(county_name),
                          county_fips = as.character(county_fips), 
                          candidate, party, 
                          votes = candidatevotes,
                          total_votes = totalvotes)]

# Add a location id
dt2 <- merge(dt, hierarchy[level==3, .(location_id, county_fips = local_id)], by='county_fips', all.x=T)

# Make some manual fixes
# Colorado
co <- dt2[state=='colorado' & county %in% c('adams', 'arapahoe', 'boulder', 'broomfield', 'denver', 'jefferson', 'weld'),
    .(votes = sum(votes),
      total_votes = sum(total_votes)), by=c('state','candidate','party')]
co$county_fips <- NA
co$county <- "weld, boulder, adams, jefferson, denver, arapahoe, broomfield"
co$location_id <- 94092
dt2 <- rbind(dt2[! (state=='colorado' & county %in% c('adams', 'arapahoe', 'boulder', 'broomfield', 'denver', 'jefferson', 'weld'))], co)

# New Mexico
nm <- dt2[state=='new mexico' & county %in% c('cibola', 'valencia'),
          .(votes = sum(votes),
            total_votes = sum(total_votes)), by=c('state','candidate','party')]
nm$county_fips <- NA
nm$county <- 'cibola, valencia'
nm$location_id <- 94066
dt2 <- rbind(dt2[! (state=='new mexico' & county %in% c('cibola','valencia'))], nm)

#Florida
dt2[state=='florida' & county == 'miami-dade', location_id:=94081]
#Montana
dt2[state=='montana' & county == 'park', location_id:=94080]
#South Dakota
dt2[state=='south dakota' & county == 'jackson', location_id:=94082]
dt2[state=='south dakota' & county == 'oglala lakota', location_id:=94084]


# Remove any rows with missing location_id
dt2 <- dt2[! is.na(location_id)]


# If 2020 data are missing, use result from 2016
# Load the raw data
dt_2016 <- fread(input_path)[year==2016 & mode=='TOTAL' & party %in% c('REPUBLICAN', 'DEMOCRAT'),
                        .(state = tolower(state), county = tolower(county_name),
                          county_fips = as.character(county_fips), 
                          candidate, party, 
                          votes = candidatevotes,
                          total_votes = totalvotes)]

# Add a location id
dt2_2016 <- merge(dt_2016, hierarchy[level==3, .(location_id, county_fips = local_id)], by='county_fips', all.x=T)

# Apply same manual fixes to 2016 data
# Colorado
co_2016 <- dt2_2016[state=='colorado' & county %in% c('adams', 'arapahoe', 'boulder', 'broomfield', 'denver', 'jefferson', 'weld'),
    .(votes = sum(votes),
      total_votes = sum(total_votes)), by=c('state','candidate','party')]
co_2016$county_fips <- NA
co_2016$county <- "weld, boulder, adams, jefferson, denver, arapahoe, broomfield"
co_2016$location_id <- 94092
dt2_2016 <- rbind(dt2_2016[! (state=='colorado' & county %in% c('adams', 'arapahoe', 'boulder', 'broomfield', 'denver', 'jefferson', 'weld'))], co_2016)

# New Mexico
nm_2016 <- dt2_2016[state=='new mexico' & county %in% c('cibola', 'valencia'),
          .(votes = sum(votes),
            total_votes = sum(total_votes)), by=c('state','candidate','party')]
nm_2016$county_fips <- NA
nm_2016$county <- 'cibola, valencia'
nm_2016$location_id <- 94066
dt2_2016 <- rbind(dt2_2016[! (state=='new mexico' & county %in% c('cibola','valencia'))], nm_2016)

# Florida
dt2_2016[state=='florida' & county == 'miami-dade', location_id:=94081]
# Montana
dt2_2016[state=='montana' & county == 'park', location_id:=94080]
# South Dakota
dt2_2016[state=='south dakota' & county == 'jackson', location_id:=94082]
dt2_2016[state=='south dakota' & county == 'oglala lakota', location_id:=94084]

# Remove any rows with missing location_id in 2016 data
dt2_2016 <- dt2_2016[! is.na(location_id)]

# Add year variable to both datasets
dt2[, year := 2020]
dt2_2016[, year := 2016]

# Identify counties with complete 2020 data (exactly 2 rows: one per party)
complete_2020 <- dt2[, .N, by = location_id][N == 2, location_id]

# For all other counties, use 2016 data as fallback
dt2_2016_fill <- dt2_2016[! location_id %in% complete_2020]

# Combine complete 2020 counties with 2016 fallback counties
final <- rbind(dt2[location_id %in% complete_2020], dt2_2016_fill)

# Keep only counties with exactly 2 rows (one per party)
final <- final[location_id %in% final[, .N, by = location_id][N == 2, location_id]]

# Select and order final columns
final <- final[, .(location_id, state, county, county_fips, year, candidate, party, votes, total_votes)]

# Save
fwrite(final, out_path)
