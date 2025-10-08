# Launch Jobs

### - SET UP - ### 

# Load packages
library(data.table)
library(yaml)
#library(stringr)
library(boot)
library(forecast)
library(zoo) #need na.locf for temp patch
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
source("/ihme/scratch/users/ems2285/thesis/code/pipeline/run_one_county.R")
source("/ihme/scratch/users/ems2285/thesis/code/pipeline/ensemble_and_forecast.R")
source("/ihme/scratch/users/ems2285/thesis/code/pipeline/model_performance_measures.R")
source("/ihme/scratch/users/ems2285/thesis/code/pipeline/adjust_UI.R")
source("/ihme/scratch/users/ems2285/thesis/code/helper_functions/resolve_loc_ids.R")
source("/ihme/scratch/users/ems2285/thesis/code/helper_functions/get_version.R")
source("/ihme/scratch/users/ems2285/thesis/code/helper_functions/daily_to_weekly.R")

# Define input data & config file paths
root_dir <- "/ihme/scratch/users/ems2285/thesis/"
config_path <- file.path(root_dir,"inputs/config_files/config.yaml")
out_dir <- file.path(root_dir, "outputs")

# Create versioned output folder
version <- get_version(out_dir)
dir.create(file.path(out_dir,version))

# Load all candidate models (each model is a function)
list_of_files <- list.files(paste0(root_dir,"/code/models"), full.names = T)
for (file in list_of_files){
  source(file)
}

# Load the location hierarchy
hierarchy <- get_location_metadata(location_set_id = 128, release_id = 9)

# Load the config file
configs <- read_yaml(config_path)

# Generate list of location_ids
counties_to_run <- resolve_loc_ids(location_type = configs$location_type,
                                   location_list = configs$location_list)

### - LAUNCH JOBS - ###
#TODO - submit one job per county-mandate type
for (mandate in configs$mandates) {
  
  # create final output 1: results_draws
  results_draws <- vector(mode="list", length=length(counties_to_run))
  names(results_draws) <- counties_to_run
  
  # create final output 2: problem_log
  problem_log <- data.table()
  
  for (county in counties_to_run){
    county_results <- run_one_county(root_dir,
                                     loc_id = county,
                                     mandate_type = mandate,
                                     configs,
                                     hierarchy)
    
    # Add the county's results to the final list
    results_draws[[as.character(county)]] <- county_results$results
    
    # Add county problems to the problem log
    problem_log <- rbind(problem_log, county_results$problem_log)
    
  }

  
  ### - ADJUST UI - ###
  # Must wait for all jobs to finish running before adjusting UI because the adjustment pools information
  # across locations.
  final_results <- adjust_UI(results_draws, problem_log, configs)
  
  
  ### - SAVE - ###
  # Save results and problem log
  saveRDS(final_results, paste0(out_dir, '/', version, '/', mandate, '_results.RDS'))
  #saveRDS(results_summary, paste0(out_dir, '/', version, '/', mandate, '_summary.RDS'))
  if(nrow(problem_log)>0){
    fwrite(problem_log, paste0(out_dir, '/', version, '/', mandate, '_problem_log.csv'))
  }

}

# save the config file to the output folder
write_yaml(configs, paste0(out_dir, '/', version, '/', 'config.yaml'))
