
#-----------------------------------------------------------------------------------------------

#Setup and packages

#-----------------------------------------------------------------------------------------------

#clear console
cat("\014")

#garbage collection
gc(verbose = FALSE, full = TRUE)

#Ncpus: The number of parallel processes to use for a parallel install of more than one source package.
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

# load CRAN packages
packages <- c(
  #external packages (not-being from cran-repo or similar)
  'devtools',
  #Python interface
  #'reticulate'
  #Functions for Base Types and Core R and 'Tidyverse' Features
  'rlang',
  #Relative paths
  'here',
  #Essentials
  'tidyverse',
  #datafraem extension
  'data.table',
  #spss
  'haven', 
  'stats',
  'rbin',
  'viridis',
  #layout plots
  #'patchwork',
  #read xlsx
  'openxlsx',
  'gtsummary',
  'survey',
  'srvyr',
  'fs',
  #chart creation
  'esquisse',
  #parallel processing
  'multidplyr', 'furr','parallel',
  #imputation
  'mice',
  #self-organised maps
  'kohonen'
)

#install packages which are not available on the computing setup
has_available   <- packages %in% rownames(installed.packages())
if(any(!has_available)) install.packages(packages[!has_available])

lapply(packages,library,character.only = TRUE
       ,quietly = TRUE
)



#-----------------------------------------------------------------------------------------------

#review packages loaded (store active-packages set-up)
sessionInfo() %>% capture.output(file="session_info.txt")
