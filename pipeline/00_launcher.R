# Launch the pipeline with real data, parameterized by context_id.
# Usage:
#   Rscript 00_launcher.R --context_ids ctx1 ctx2 ctx3
#   Rscript 00_launcher.R --context_ids all   # run every context in the lookup table

library(argparse)
library(data.table)

# Define root directory
code_dir  <- "/ihme/homes/ems2285/repos/simulate-pipeline"
input_dir <- "/ihme/scratch/users/ems2285/thesis/aim_3/processed_data/"
out_root  <- "/ihme/scratch/users/ems2285/thesis/outputs/outputs/"
config_dir <- "/ihme/scratch/users/ems2285/thesis/inputs/config_files/"

source(file.path(code_dir, 'helper_functions/get_version.R'))
source(file.path(code_dir, 'helper_functions/set_up.R'))
source(file.path(code_dir, 'helper_functions/submit_jobs.R'))

## 0. PARSE ARGUMENTS ##
parser <- ArgumentParser()
parser$add_argument(
  "--context_ids",
  type    = 'character',
  nargs   = '+',
  help    = "One or more context IDs to run, or 'all' to run every context in the lookup table."
)
args       <- parser$parse_args()
context_ids <- args$context_ids

## 1. SETUP OUTPUT DIRECTORY ##
out_dir <- set_up(out_root)

## 2. LOAD LOOKUP TABLES ##
locs_by_context <- fread(file.path(config_dir, 'locs_by_context.csv'))
context_lookup  <- fread(file.path(config_dir, 'context_lookup_table.csv'))

# Support 'all' as a shorthand for every context
if (length(context_ids) == 1 && context_ids == "all") {
  context_ids <- context_lookup$context_id
}

# Validate requested contexts
missing_ctx <- setdiff(context_ids, context_lookup$context_id)
if (length(missing_ctx) > 0) {
  stop("The following context_ids were not found in context_lookup_table.csv: ",
       paste(missing_ctx, collapse = ", "))
}

# Filter to requested contexts (preserve the requested order)
context_lookup  <- context_lookup[context_id %in% context_ids]
locs_by_context <- locs_by_context[context_id %in% context_ids]

n_contexts <- nrow(context_lookup)
message("Launching ", n_contexts, " context(s): ", paste(context_lookup$context_id, collapse = ", "))

## 3. SAVE LOOKUP TABLES AND CONTEXT INDEX TO OUTPUT DIR ##
# Downstream jobs read these files to resolve their context.
fwrite(locs_by_context, file.path(out_dir, 'inputs/locs_by_context.csv'))
fwrite(context_lookup,  file.path(out_dir, 'inputs/context_lookup_table.csv'))

# Write an ordered list of context IDs (one per line).
# SLURM_ARRAY_TASK_ID (1-indexed) maps to line N of this file.
writeLines(as.character(context_lookup$context_id),
           file.path(out_dir, 'inputs/contexts.txt'))

## 4. LAUNCH ARRAY JOB (one task per context) ##
sbatch(
  jobname  = 'its_pipeline',
  code     = file.path(code_dir, "pipeline/0_pipeline_wrapper.R"),
  pass     = paste0('--code_dir ',  code_dir,
                    ' --input_dir ', input_dir,
                    ' --out_dir ',   out_dir),
  mem      = '6G',
  fthreads = 4,
  q        = "all.q",
  arr_len  = n_contexts,
  log      = TRUE,
  e        = file.path(out_dir, 'logs/error_context_%a.txt'),
  o        = file.path(out_dir, 'logs/out_context_%a.txt'),
  submit   = TRUE
)

## 5. QC REMINDER ##
# After all jobs finish, verify that expected output files exist:
#   files <- list.files(file.path(out_dir, 'batched_output'), pattern = 'pred_adj')
#   if (length(files) < n_contexts) message('Some output files are missing')
