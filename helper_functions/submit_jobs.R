## jobname:      Name for job, must start with a letter - required
## code:         Filepath to code file - required
## hold:         Comma-separated list of jobnames to hold the job on
## pass:         Vector of arguments to pass on to receiving script
## mem:          Maximum memory allotment - required
## fthreads:     Maximum thread allotment - required
## max_run_time: Maximum job runtime - each queue has a default; format is HH:MM:SS
## q:            What queue to get in?
## proj:         What is the project flag to be used?
## archive:      Does the job need J access?
## log:          Should this job create a log for errors and output?
## error_file:   where do you want to save your error files (log should be T if you use this)
## output_file:  where do you want to save your output files (log should be T if you use this)
## arr_len:      If not NULL, how many array jobs?
## shell:        What shell script to use.
## sing_ver:     If you want to specify the version of the singularity you are using
## submit:       Should we actually submit this job, or do you just want to have a qsub printed that you can submit yourself


sbatch <- function(jobname,
                   code,
                   hold=NULL,
                   pass=NULL,
                   mem,
                   fthreads,
                   max_run_time = NULL,
                   q = "all.q",
                   archive = F,
                   proj = "proj_lsae",
                   log=T,
                   error_file = "errors.txt",
                   output_file = "output.txt",
                   arr_len=NULL,
                   shell = NULL,
                   sing_ver = NULL,
                   submit=F
) {
  
  if(is.null(shell)) {
    shell <- '/ihme/singularity-images/rstudio/shells/execRscript.sh'
  }
  
  queue <- paste0("-p ", q)
  
  # set up jobs to hold for
  if (!is.null(hold)) {
    hold.string <- paste(" -d \"", hold, "\"", sep="")
  }
  # set up arguments to pass in
  if (!is.null(pass)) {
    pass.string <- ""
    for (ii in pass) pass.string <- paste(pass.string, " \"", ii, "\"", sep="")
  }
  # construct the command
  sub <- paste("sbatch ",
               queue,
               " -J ", jobname, " ",
               paste0(" --mem=", mem, " "),
               paste0(" -c ", fthreads, " "),
               if (!is.null(max_run_time)) paste0(" -t ", max_run_time, " "),
               if (archive==T) paste0(" -C archive "),
               if(proj != "") paste0(" -A ",proj," "),
               if (!is.null(arr_len)) paste0(" -a 1-", arr_len, " "),
               if(log==F) " -e /dev/null -o /dev/null ",
               if(log==T) paste0(" -e ", error_file, " -o ", output_file, " "),
               if (!is.null(hold)) hold.string,
               shell, " ",
               if(!is.null(sing_ver)) paste0("-i ", sing_ver, " "),
               "-s ", code, " ",
               if (!is.null(pass)) pass.string,
               sep="")
  # submit the command to the system
  if (submit) {
    system(sub)
  } else {
    cat(paste("\n", sub, "\n\n "))
    flush.console()
  }
}