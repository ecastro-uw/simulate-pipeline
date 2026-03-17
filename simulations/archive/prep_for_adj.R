# Prep for adj

prep_for_adj <- function(results, pipeline_inputs){
  
  
  for (r in 1:R)
  final_results <- adjust_UI(results_draws, problem_log, pipeline_inputs$configs)
  
  return(final_results)
}