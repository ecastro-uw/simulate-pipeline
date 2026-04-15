# Interactive script to explore context definitions and how
# various cut-points impact bin sizes

# For each of the four imposition categories (1st/2nd restaurant/bar), group counties
# by size (big/small), political affiliation (rep/dem/mod), and mandate timing.

# Load packages/funcs
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# Load hierarchy
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)

# Define dirs
input_root <- '/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/USA_counties/'

# Define args
mandate_type <- 'restaurant'
mandate_num <- 'first'
padding <- 2 #discard 2 weeks of data after the mandate lifts


# load population data
#mcnty_pop <- fread("/mnt/share/dex/us_county/03_post_model/pop_denom/best/inputs/county_population_age_sex.csv")[year_id == 2020][, sum(pop, na.rm = T), by = 'mcnty']
#setnames(mcnty_pop,'V1','pop')
#TODO - need mcounty to location_id mapping
pop_dt <- fread("/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/USA_counties/population.csv")
pop_dt[, pop_cat := ifelse(pop>=100000, 'big', 'small')]

# load political affiliation data
elect_dt <- fread("/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/USA_counties/election_results.csv")
elect_dt[, pct := votes/total_votes]
elect_wide <- dcast(elect_dt, location_id ~ party, value.var = 'pct')
elect_wide[, pol_cat := ifelse(DEMOCRAT>0.55, 'D', ifelse(REPUBLICAN>0.55, 'R', 'M'))]

# load event timing data
event_dt <- fread(paste0(input_root, mandate_num,'_',mandate_type,'_close.csv')) 

# For second impositions, drop events with insufficient interval between first lift and second imposition
if(mandate_num=='second'){
  event_dt <- event_dt[floor((onset_date - prev_lift)/7)>=10]
}

# combine data
event_dt <- merge(event_dt[,.(location_id, onset_date)], pop_dt[, .(location_id, pop_cat)], by='location_id', all.x=T)
event_dt <- merge(event_dt, elect_wide, by='location_id', all.x=T)

# check for missingness
print(paste(nrow(event_dt[is.na(pop_cat)]), "counties are missing population data"))
print(paste(nrow(event_dt[is.na(pol_cat)]), "counties are missing political party data"))
event_dt <- event_dt[! is.na(pop_cat) & ! is.na(pol_cat)]

# categorize timing
if(mandate_num=='first'){
  
  # Define timing bin
  event_dt$timing_cat <- 'Mar-Apr 2020'
  
  # Calc number of locations per bin
  summary <- event_dt[,.N, by=c('pop_cat', 'pol_cat')]
  summary_wide <- dcast(summary, pop_cat ~ pol_cat, value.var = 'N')
}

if(mandate_num=='second'){
  
  # Define timing bin
  event_dt[, month := month(onset_date)]
  event_dt[, year := year(onset_date)]
  if(mandate_type=='restaurant'){
    event_dt[, timing_cat := ifelse(year==2020 & month %in% 6:11, 'Jun-Nov 2020',
                                    ifelse((year==2020 & month==12) | (year==2021 & month %in% 1:5), 'Dec 2020-May 2021', NA))]
  } else {
    event_dt$timing_cat <- 'Jun 2020-May 2021'
  }
  # Calc number of locations per bin
  summary <- event_dt[,.N, by=c('timing_cat', 'pop_cat', 'pol_cat')]
  summary_wide <- dcast(summary[! is.na(timing_cat)], 
                        pop_cat ~ timing_cat + pol_cat)
}