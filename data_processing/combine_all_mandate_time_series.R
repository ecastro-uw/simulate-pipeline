# Create a file that's long by location-day and wide by mandate

# libs
library(data.table)

# directories
mandate_dir <- "/snfs1/Project/covid/data_intake_prod/social distancing/diagnostic outputs/alternate_prepped_outputs/prepped_outputs_counties/"
out_dir <- "/ihme/scratch/users/ems2285/thesis/intermediate/"

# list of mandates
mandate_list <- c('bar_close', 'dining_close', 'gatherings50i100o', 'gatherings9998iON', 'gym_pool_leisure_close',
                  'primary_edu', 'secondary_edu', 'higher_edu', 'stay_at_home', 'non_essential_retail_close')

one_mandate <- function(mandate_name){
  temp_dt <- fread(paste0(mandate_dir, mandate_name, '.csv'))[location_id %in% counties$location_id,.(location_id, date, get(mandate_name))]
  temp_dt[, date := as.Date(date, format='%d.%m.%Y')]
  setnames(temp_dt, 'V3', 'status')
  return(temp_dt)
}

temp_mand <- data.table(file = 1:length(mandate_list), mandate=mandate_list)

mandate_dt <- rbindlist(lapply(mandate_list, one_mandate), use.names=F, idcol="file")
mandate_dt <- merge(mandate_dt, temp_mand, by='file', all.x=T)
mandate_wide <- dcast(mandate_dt, location_id + date ~ mandate, value.var = 'status')
fwrite(mandate_wide, paste0(out_dir, 'all_cnty_mandates.csv'))