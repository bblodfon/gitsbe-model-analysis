library(testthat) # version: 2.0.1

# Make sure that the working directory is set to
# the Rscripts folder: setwd("pathTo/Rscripts")
# setwd(paste0(getwd(),"/Rscripts"))

source("input_functions.R")
source("analysis_functions.R")

test_dir("test")
