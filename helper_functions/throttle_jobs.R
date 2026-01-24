# Job throttling utilities for SLURM
# These functions help limit the number of concurrent jobs submitted

#' Count current running and pending jobs for the user
#'
#' @param user Username to check jobs for. Defaults to current user.
#' @param job_prefix Optional prefix to filter jobs by name.
#' @return Integer count of running and pending jobs
count_user_jobs <- function(user = Sys.getenv("USER"), job_prefix = NULL) {
  # Build squeue command to count jobs
  cmd <- sprintf("squeue -u %s -h -t RUNNING,PENDING", user)

  # If job_prefix specified, filter by job name
  if (!is.null(job_prefix)) {
    cmd <- paste0(cmd, sprintf(" -n %s", job_prefix))
  }

  # Count lines of output (each line = one job)
  cmd <- paste0(cmd, " | wc -l")

  result <- system(cmd, intern = TRUE)
  as.integer(trimws(result))
}

#' Wait until there's a slot available for a new job
#'
#' @param max_jobs Maximum number of concurrent jobs allowed
#' @param user Username to check jobs for. Defaults to current user.
#' @param job_prefix Optional prefix to filter jobs by name.
#' @param check_interval Seconds to wait between checks. Default 30.
#' @param verbose Print status messages. Default TRUE.
wait_for_slot <- function(max_jobs,
                          user = Sys.getenv("USER"),
                          job_prefix = NULL,
                          check_interval = 30,
                          verbose = TRUE) {

  current_jobs <- count_user_jobs(user = user, job_prefix = job_prefix)

  while (current_jobs >= max_jobs) {
    if (verbose) {
      message(sprintf("[%s] %d jobs running/pending (limit: %d). Waiting %d seconds...",
                      format(Sys.time(), "%H:%M:%S"),
                      current_jobs, max_jobs, check_interval))
    }
    Sys.sleep(check_interval)
    current_jobs <- count_user_jobs(user = user, job_prefix = job_prefix)
  }

  if (verbose) {
    message(sprintf("[%s] Slot available (%d/%d jobs). Submitting...",
                    format(Sys.time(), "%H:%M:%S"),
                    current_jobs, max_jobs))
  }

  invisible(current_jobs)
}
