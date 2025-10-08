library(stringr)

get_version <- function(filepath){
  # Get today's date
  date <- gsub('-', '', Sys.Date())
  
  # Generate a list of all output folders with todays date
  list_of_folders <- list.files(filepath, pattern=date)
  
  # Determine the version id
  if(length(list_of_folders)==0){
    vv <- '01'
  } else{
    # Determine the max version id and add 1 to get the new version id
    vv <- max(as.numeric(str_sub(list_of_folders,start=-2))) + 1
    vv <- ifelse(vv<10, paste0('0',vv), as.character(vv))
  }
  
  # Combine date and version id to get the version
  version <- paste0(date,'.',vv)
  
  # Return the result
  return(version)
}