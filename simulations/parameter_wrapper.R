# Parameter wrapper
# Enumerate all parameter combinations and launch one combination at a time

parameter_wrapper <- function(out_dir, root_dir){
  
  # Load parameter file
  params <- fread(paste0(out_dir, '/params.csv'))
  
  # Launch once for each parameter set
  for (i in 1:nrow(params)){
    batch_wrapper()
  }
}