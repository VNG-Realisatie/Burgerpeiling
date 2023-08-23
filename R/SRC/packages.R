
#-----------------------------------------------------------------------------------------------

#Setup and packages

#-----------------------------------------------------------------------------------------------

#clear console
cat("\014")

#garbage collection
gc(verbose = FALSE, full = TRUE)

#Ncpus: The number of parallel processes to use for a parallel install of multiple packages.
options(Ncpus = 8) #we use 8 instead of 1
#getOption("Ncpus", 1L)

message("deploy packages")

if(proj_env == TRUE) {
  #containerized packages (in case you encounter issue with the current version of packages within your computing set-up)
  if (!require("renv")) install.packages("renv")
  library("renv")
  renv::upgrade() # upgrades renv, if new version is available
  renv::update() # updates packages from CRAN and GitHub, within the project
  renv::hydrate(update = "all") # populates the renv cache with copies of-up-to-date packages
  renv::snapshot() # inspect the message before confirming to overwrite renv.lock
  renv::init() #let's go!
}

#-----------------------------------------------------------------------------------------------

# load required libraries
libraries <- c(
  #external packages (not-being from cran-repo or similar)
  'devtools',
  #Functions for Base Types and Core R and 'Tidyverse' Features
  'rlang',
  #tools
  'tools',
  #Relative paths
  'here',
  #sssentials
  'tidyverse','janitor','scales',
  #dataframe extension
  'data.table',
  #spss
  'haven', 'labelled',
  #Read Rectangular Text Data
  'readr',
  #statistical calculations
  'stats',
  #multiuple response sets
  'expss',
  #colour scheme
  'viridis',
  #layout plots
  #'patchwork',
  #read and write xlsx
  'openxlsx', 
  #publication-ready analytical and summary tables
  'gtsummary',
  #survey data-processing
  'survey',
  'srvyr',
  #file system operations
  'fs',
  #CBS api
  'cbsodataR',
  #scales
  'scales',
  #chart creation
  'esquisse',
  #parallel processing
  'multidplyr', 
  #mapping functions for parallel processing
  #'furr',
  'parallel',
  #imputation
  'mice',
  #self-organised maps
  'kohonen',
  #ensemble clustering
  'diceR',
  #optimal binning
  'optbin'
)

# Install and load missing packages
missing_libraries <- libraries[!libraries %in% installed.packages()]
if (length(missing_libraries) > 0) {
  install.packages(missing_libraries)
}
lapply(libraries, library, character.only = TRUE, quietly = TRUE)


# Load the furrr package for parallel processing
if (!requireNamespace("furrr", quietly = TRUE)) {
  #install.packages("furrr")
  devtools::install_github("DavisVaughan/furrr")
}
library(furrr)

#-----------------------------------------------------------------------------------------------

#review packages loaded (store active-packages set-up)
sessionInfo() %>% capture.output(file="session_info.txt")