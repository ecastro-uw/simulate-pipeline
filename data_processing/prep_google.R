library(data.table)

input_path <- "/mnt/team/covid_19/pub/model-inputs/2022_12_13.03/mobility/google_mobility_with_locs.csv"
out_root <- "/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/"

# Load the data
vars_to_keep <- c('location_id', 'location_name', 'date', 'country_region', 'sub_region_1', 'sub_region_2',
                  'retail_and_recreation_percent_change_from_baseline', 'retail_imputed')
dt <- fread("/mnt/team/covid_19/pub/model-inputs/2022_12_13.03/mobility/google_mobility_with_locs.csv")[, .SD, .SDcols = vars_to_keep]
dt[, date:=as.Date(date, format = "%d.%m.%Y")]

# 1. US States
US_states <- dt[country_region %in% 'United States' & sub_region_1!="" & sub_region_2==""]
# Drop special WA locs 
US_states <- US_states[! location_id %in% c(60886, 60887)] 
#Subset to smaller date range
US_states <- US_states[date < '2021-06-01', .(location_id, date, retail_and_recreation_percent_change_from_baseline)]

# 2. US Counties
US_counties <- dt[country_region %in% 'United States' & sub_region_2!="" & date < '2021-06-01']

# 3. BR States
BR_states <- dt[country_region %in% 'Brazil' & sub_region_1!="" & sub_region_2=="" & date < '2021-06-01']

# 4. BR Municipalities
BR_municipalities <- dt[country_region %in% 'Brazil' & sub_region_2!="" & date < '2021-06-01']

# Save
fwrite(US_states, paste0(out_root,'USA_states/google_mobility.csv'))
fwrite(US_counties, paste0(out_root,'USA_counties/google_mobility.csv'))
fwrite(BR_states, paste0(out_root,'Brazil_states/google_mobility.csv'))
fwrite(BR_municipalities, paste0(out_root,'Brazil_states/google_mobility_municipalities.csv'))
