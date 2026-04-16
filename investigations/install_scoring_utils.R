
# tried updating ~/.bashrc and ~/.Renviron 
Sys.getenv('R_LIBS')
Sys.getenv('R_LIBS_USER')
.libPaths()

# add the rpackages directory to my libPaths
lib_dir <- file.path(Sys.getenv("HOME"), "rpackages")
.libPaths(c(.libPaths(),lib_dir))
.libPaths()



# confirm reading the .Renviron in my homes directory
Sys.getenv("R_ENVIRON_USER")


# Check which packages are installed
find.package("scoringRules", lib.loc = .libPaths())
installed.packages(lib.loc = .libPaths())[, c("Package", "LibPath")]

# Try install
install.packages('scoringutils',
                 lib='/ihme/homes/ems2285/rpackages')


# try using callr
library(callr)
withr::with_libpaths(
  new = "/ihme/homes/ems2285/rpackages"
  , devtools::install_cran(pkgs = "scoringutils", dependencies = F)
)

withr::with_libpaths(
  new = "/ihme/homes/ems2285/rpackages"
  , devtools::install_github(repo = "epiforecasts/scoringutils", dependencies = FALSE)
)
