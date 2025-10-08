# convert daily data to weekly

daily_to_weekly <- function(row, county_dt, configs) {

  # Set some values
  w <- configs$w
  max_train_t <- row$max_train_t
  
  # Subset the data (per the max # of training weeks)
  dt <- county_dt[date >= row$onset - 7*max_train_t & date < row$onset + 7*w]
  
  # TEMP FIX
  # check that all expected dates are present
  if(nrow(dt)<(max_train_t+w)*7){
    # fill in missing days
    new_dt <- merge(dt, data.table(date=seq(row$onset - 7*max_train_t, row$onset -1 + 7*w, by="days")), by='date', all.y=T)
    # impute missing values using LOCF
    new_dt <- na.locf(new_dt)
    dt <- new_dt[,.(location_id, fips, county, date, y, mandate)]
  }
  
  # Aggregate data to the week level
  dt[, time_id := rep(-max_train_t:(w-1), each=7)]
  dt <- dt[, .(y = sum(y)), by=time_id]

  return(dt)
}