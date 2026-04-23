# Prep election data
# Source: https://electionlab.mit.edu/data 

# Load libs
library(data.table)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# Define dirs
input_path <-       "/ihme/scratch/users/ems2285/thesis/inputs/data/countypres_2000-2024_MIT.csv"
input_state_lvl <-  "/ihme/scratch/users/ems2285/thesis/inputs/data/1976-2020-president_MIT.csv"
out_path <-         "/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/USA_counties/election_results.csv"
output_state_lvl <- "/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/USA_states/election_results.csv"

# load the location hierarchy
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)


### COUNTIES ###
# Load the raw data
dt <- fread(input_path)[year==2020 & mode=='TOTAL' & party %in% c('REPUBLICAN', 'DEMOCRAT'),
                        .(state = tolower(state), county = tolower(county_name),
                          county_fips = as.character(county_fips), year,
                          candidate, party, 
                          votes = candidatevotes,
                          total_votes = totalvotes)]

# Add a location id
dt2 <- merge(dt, hierarchy[level==3, .(location_id, county_fips = local_id)], by='county_fips', all.x=T)

# Make some manual fixes
# Colorado
co <- dt2[state=='colorado' & county %in% c('adams', 'arapahoe', 'boulder', 'broomfield', 'denver', 'jefferson', 'weld'),
    .(votes = sum(votes),
      total_votes = sum(total_votes)), by=c('state','year','candidate','party')]
co$county_fips <- NA
co$county <- "weld, boulder, adams, jefferson, denver, arapahoe, broomfield"
co$location_id <- 94092
dt2 <- rbind(dt2[! (state=='colorado' & county %in% c('adams', 'arapahoe', 'boulder', 'broomfield', 'denver', 'jefferson', 'weld'))], co)

# New Mexico
nm <- dt2[state=='new mexico' & county %in% c('cibola', 'valencia'),
          .(votes = sum(votes),
            total_votes = sum(total_votes)), by=c('state','year','candidate','party')]
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
                          county_fips = as.character(county_fips), year,
                          candidate, party, 
                          votes = candidatevotes,
                          total_votes = totalvotes)]

# Add a location id
dt2_2016 <- merge(dt_2016, hierarchy[level==3, .(location_id, county_fips = local_id)], by='county_fips', all.x=T)

# Make some manual fixes
# Arizona
az <- dt2_2016[state=='arizona' & county %in% c("la paz", "yuma"),
               .(votes = sum(votes),
                 total_votes = sum(total_votes)), by=c('state', 'year','candidate','party')]
az$county_fips <- NA
az$county <- "la paz, yuma"
az$location_id <- 94063
dt2_2016 <- rbind(dt2_2016[! (state=='arizona' & county %in% c("la paz", "yuma"))], az)

# Maryland
md <- dt2_2016[state=='maryland' & county %in% c("montgomery", "prince george's"),
          .(votes = sum(votes),
            total_votes = sum(total_votes)), by=c('state','year','candidate','party')]
md$county_fips <- NA
md$county <- "montgomery, prince george's"
md$location_id <- 94065
dt2_2016 <- rbind(dt2_2016[! (state=='maryland' & county %in% c("montgomery", "prince george's"))], md)

# Virginia
# Albemarle & Charlottesville
dt2_2016[state=='virginia' & county %in% c("fairfax", "fairfax"),
         location_id := 94067]
dt2_2016[state=='virginia' & county %in% c("rockingham", "harrisonburg"),
         location_id := 94068]
dt2_2016[state=='virginia' & county %in% c("spotsylvania", "fredericksburg"),
         location_id := 94069]
dt2_2016[state=='virginia' & county %in% c("albemarle", "charlottesville"),
         location_id := 94070]
dt2_2016[state=='virginia' & county %in% c("rockbridge", "buena vista"),
         location_id := 94071]
dt2_2016[state=='virginia' & county=='bedford',
         location_id := 94072]
dt2_2016[state=='virginia' & county %in% c("williamsburg", "james city"),
         location_id := 94073]
dt2_2016[state=='virginia' & county %in% c("york", "newport news"),
         location_id := 94074]
dt2_2016[state=='virginia' & county %in% c("greensville", "emporia"),
         location_id := 94075]
dt2_2016[state=='virginia' & county %in% c("southampton", "franklin") & county_fips != 51067,
         location_id := 94076]
dt2_2016[state=='virginia' & county %in% c("pittsylvania", "danville"),
         location_id := 94077]
dt2_2016[state=='virginia' & county=='alleghany',
         location_id := 94078]
dt2_2016[state=='virginia' & county=='halifax',
         location_id := 94079]
dt2_2016[state=='virginia' & county %in% c("prince william", "manassas park", "manassas"),
         location_id := 94088]
dt2_2016[state=='virginia' & county %in% c("augusta", "staunton", "waynesboro"),
         location_id := 94089]

dt2_2016 <- dt2_2016[! is.na(location_id), .(year=unique(year),
                                 votes = sum(votes),
                                 total_votes = sum(total_votes)),
                     by=c('location_id', 'candidate', 'party')]

# Identify counties with complete 2020 data (exactly 2 rows: one per party)
complete_2020 <- dt2[, .N, by = location_id][N == 2, location_id]

# For all other counties, use 2016 data as fallback
dt2_2016_fill <- dt2_2016[! location_id %in% complete_2020]

# Combine complete 2020 counties with 2016 fallback counties
final <- rbind(dt2[location_id %in% complete_2020, .(location_id, party, year, candidate, votes, total_votes)],
               dt2_2016_fill)

# Keep only counties with exactly 2 rows (one per party)
final <- final[location_id %in% final[, .N, by = location_id][N == 2, location_id]]

# Save
fwrite(final, out_path)




### STATES ###
# Load the raw data
dt <- fread(input_state_lvl)[year==2020 & party_simplified %in% c('REPUBLICAN', 'DEMOCRAT'),
                        .(state = tolower(state),
                          party = party_simplified,
                          candidate,
                          votes = candidatevotes,
                          total_votes = totalvotes)]

# Add a location id
dt2 <- merge(dt, hierarchy[level==2, .(location_id, state=tolower(location_name))], by='state', all.x=T)
dt2 <- dt2[, .(location_id, party, candidate, votes, total_votes)]

# Save
fwrite(dt2, output_state_lvl)
