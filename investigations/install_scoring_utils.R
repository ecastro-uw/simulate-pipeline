
# Step 1: update .Rprofile file in your homes directory
#.libPaths(c("~/lib_for_scoringutils", .libPaths()))
#
# Be sure to leave an empty space afterwards, at the end of the .Rprofile!

# Install the package
install.packages('scoringutils', lib='/ihme/homes/ems2285/lib_for_scoringutils')

# May need to try this first
install.packages('scoringRules', lib='/ihme/homes/ems2285/lib_for_scoringutils')

# Load the package
library(scoringRules, lib.loc = '/ihme/homes/ems2285/lib_for_scoringutils')



############
# define your dir
dir <- "/mnt/share/homes/ems2285/lib_for_scoringutils"  

# attempt to install the package
install.packages('scoringutils', 
                 lib=dir, 
                 dependencies=T


install.packages("rlang", lib = dir)                    # this version needs to be correct for scoringutils to work correctly when called
install.packages("rlang", lib = "~/rlib/4.5.2")         # escape hatch just for installation (required for vctrs) - depends on which singularity image you're on, so keep track of that.
install.packages("vctrs", lib = dir)                    # needs to find the most current version (the singularity image version is too old)
install.packages("scoringRules", lib = "~/rlib/4.5.2")  # same issue as rlang
install.packages("scoringutils", lib = dir)             # cross fingers
library('scoringutils', lib.loc = dir)                  # profit
############

# add the rpackages directory to my libPaths
lib_dir <- file.path(Sys.getenv("HOME"), "rpackages")
.libPaths(c(.libPaths(),lib_dir))
.libPaths()

# Check which packages are installed
find.package("scoringRules", lib.loc = .libPaths())
installed.packages(lib.loc = .libPaths())[, c("Package", "LibPath")]

