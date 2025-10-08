resolve_loc_ids <- function(location_type, location_list, loc_hierarchy = hierarchy){
  if (location_type=='county'){
    # check that only county location_ids have been provided
    if(sum(location_list %in% hierarchy[level!=3, location_id])>0){
      stop("At least one location id provided is not a county. Please check the config file.")
    }
    counties_to_run <- location_list
  } else if (location_type=='state'){
    # check that only state location_ids have been provided
    if(sum(location_list %in% hierarchy[level!=2, location_id])>0){
      stop("At least one location id provided is not a state. Please check the config file.")
    }
    counties_to_run <- loc_hierarchy[parent_id %in% loc_hierarchy[location_name %in% location_list, location_id], location_id]
  } else { #ALL COUNTIES
    counties_to_run <- loc_hierarchy[level==3, location_id]
  }
  return(counties_to_run)
}