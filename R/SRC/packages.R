
#-----------------------------------------------------------------------------------------------

# Setup and packages

#-----------------------------------------------------------------------------------------------

# Clear console
cat("\014")

# Garbage collection
gc(verbose = FALSE, full = TRUE)

# Detect the number of available CPU cores minus one core for system stability
cpu_cores <- parallel::detectCores() - 1
options(Ncpus = cpu_cores)
cat(sprintf("CPU cores dedicated to this pipeline: %d\n", getOption("Ncpus", 1L)),"\n")

# Set package repository platform
if (!use_renv)
  options(repos = c(CRAN = "https://cran.r-project.org"))

message("Loading packages...")

if (use_renv) {
  # Install and load the 'renv' package (if not already installed)
  if (!requireNamespace("renv", quietly = TRUE)) {
    utils::install.packages("renv")
  }
  
  # Restore packages as recorded in the lockfile
  renv::restore()
}

#-----------------------------------------------------------------------------------------------

# Define vector of all to-be-loaded packages:
libraries <- c(
  # Functions for base types, core R, and 'tidyverse' features:
  'rlang',
  # Tools:
  'tools',
  # Relative paths:
  'here',
  # Essentials:
  'tidyverse','janitor','scales',
  # Dataframe extension:
  'data.table',
  # SPSS
  'haven', 'labelled',
  # Read rectangular text data:
  'readr',
  # Statistical calculations:
  'stats',
  # Multiple response sets:
  'expss',
  # Colour scheme
  'viridis',
  # Layout plots (not used):
  #'patchwork',
  # Read and write .xlsx files:
  'openxlsx', 
  # Publication-ready analysis and summary tables:
  'gtsummary',
  # Survey data-processing:
  'survey',
  'srvyr',
  # File system operations:
  'fs',
  # CBS API:
  'cbsodataR',
  # Scales:
  'scales',
  # Chart creation:
  'esquisse',
  # Parallel processing:
  'multidplyr', 
  # Mapping functions for parallel processing:
  'furrr',
  'parallel',
  # Multiple imputation for missing values:
  'mice',
  # Self-organised maps:
  'kohonen',
  # Ensemble clustering:
  'diceR',
  # Optimal binning:
  'optbin'
)

# Install and load missing packages
if (!use_renv) {
  missing_libraries <- libraries[!libraries %in% installed.packages()]
  if (length(missing_libraries) > 0) {
    install.packages(missing_libraries)
  }
}

# Load packages
lapply(libraries, library, character.only = TRUE, quietly = TRUE)

#-----------------------------------------------------------------------------------------------

# Review packages loaded and write to file (i.e., store active-packages set-up)
sessionInfo() %>% capture.output(file="session_info.txt")

message("Done loading packages.")