lib_dir <- file.path(Sys.getenv("HOME"), "rpackages")
dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
install.packages('scoringutils',
                 lib = lib_dir,
                 repos = 'https://cloud.r-project.org')
.libPaths(c(lib_dir, .libPaths()))