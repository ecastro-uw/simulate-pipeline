
set_up <- function(root_dir){

  # Define dirs
  out_root <- file.path(root_dir, "outputs/simulations")
  
  # Create output folders
  out_dir <- file.path(out_root, get_version(out_root))
  dir.create(out_dir)
  dir.create(file.path(out_dir, 'inputs'))
  dir.create(file.path(out_dir,'batched_output'))
  dir.create(file.path(out_dir,'logs'))
  
  return(out_dir)
}