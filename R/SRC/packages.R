#-----------------------------------------------------------------------------------------------

# Setup and packages

#-----------------------------------------------------------------------------------------------

# clear console
cat("\014")

# garbage collection
gc(verbose = FALSE, full = TRUE)

# detect the number of available CPU cores minus one core for system stability
cpu_cores <- parallel::detectCores() - 1
options(Ncpus = cpu_cores)
cat(sprintf("CPU cores dedicated to this pipeline: %d\n", getOption("Ncpus", 1L)), "\n")

# package repository platform
options(repos = c(CRAN = "https://cran.r-project.org"))

message("deploy packages")

if (use_renv) {
  # install and load the renv package (if not already installed)
  if (!requireNamespace("renv", quietly = TRUE)) {
    utils::install.packages("renv")
  }
  library(renv)

  # initialize or create an renv.lock file
  if (!file.exists("renv.lock")) {
    renv::init()
  }
}

#-----------------------------------------------------------------------------------------------

# load required libraries
libraries <- c(
  # external packages (not-being from cran-repo or similar)
  "devtools",
  # Functions for Base Types and Core R and 'Tidyverse' Features
  "rlang",
  # tools
  "tools",
  # Relative paths
  "here",
  # sssentials
  "tidyverse", "janitor", "scales",
  # dataframe extension
  "data.table", "purrr",
  # spss
  "haven", "labelled",
  # Read Rectangular Text Data
  "readr",
  # statistical calculations
  "stats",
  # multiuple response sets
  "expss",
  # colour scheme
  "viridis",
  # layout plots
  #' patchwork',
  # read and write xlsx
  "openxlsx",
  # publication-ready analytical and summary tables
  "gtsummary",
  # survey data-processing
  "survey",
  "srvyr",
  # file system operations
  "fs",
  # mapping functions
  "furrr",
  # CBS api
  "cbsodataR",
  # scales
  "scales",
  # chart creation
  "esquisse",
  # parallel processing
  "multidplyr",
  # mapping functions for parallel processing
  #' furr',
  "parallel",
  # imputation
  "mice",
  # self-organised maps
  "kohonen",
  # ensemble clustering
  "diceR",
  # optimal binning
  "optbin"
)

# Install and load missing packages
missing_libraries <- libraries[!libraries %in% installed.packages()]
if (length(missing_libraries) > 0) {
  install.packages(missing_libraries)
}
lapply(libraries, library, character.only = TRUE, quietly = TRUE)


# Load the furrr package for parallel processing
if (!requireNamespace("furrr", quietly = TRUE)) {
  # install.packages("furrr")
  devtools::install_github("DavisVaughan/furrr")
}
library(furrr)

# update the renv.lock file with the package dependencies
if (use_renv) {
  renv::snapshot()
}

# optionally restore the environment with the specified packages
# renv::restore()

#-----------------------------------------------------------------------------------------------

# review packages loaded (store active-packages set-up)
sessionInfo() %>% capture.output(file = "session_info.txt")
