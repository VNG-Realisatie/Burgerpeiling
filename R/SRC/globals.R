
#-----------------------------------------------------------------------------------------------

#Globals and constants

#-----------------------------------------------------------------------------------------------

message("Globals and constants")

#OS
system <- Sys.info()['sysname']

# encoding
Sys.getlocale()

#getOption("encoding")

#turn-off R’s automatic conversion of strings into factors
#prevent exponential / scientific notation.
#decimal delimiter
#encoding
#warnings
#turn-off dplyr's summarise notifications
#skip computations with missings and pass over missing values 
#use all cores 
#truncate output

options(scipen = 10, digits = 4, OutDec=",", stringsAsFactors = FALSE, encoding = "UTF-8",
        warn = 0, dplyr.summarise.inform = FALSE,na.action = "na.pass",mc.cores = parallel::detectCores(),
        max.print = 50)


#-----------------------------------------------------------------------------------------------


#R root
r_root <- here::here()

#set working directory
#setwd(r_root)

#directories
#create directories on-the-fly if not exist

#location data
data.dir <- here::here("DATA")

ref.dir <- here::here("DATA","REF")

#location output
output.dir <- here::here("OUTPUT")

#location plots
plots.dir <- here::here("PLOTS")

#location data
report.dir <- here::here("REPORT")

#CBS data
cbs.dir<- here::here(data.dir, "CBS")

#create locations if not exist
locations <- c(data.dir,
               cbs.dir,
               output.dir,
               report.dir)

lapply(locations, function(x) {
  if (!dir.exists(x)) {dir.create(x)}
})


#clear plots and data directory
#clear_locations <- c(plots.loc,data.loc,report.loc)

# get all files in the directories, recursively
#f <- list.files(clear_locations, include.dirs = F, full.names = T, recursive = T)

# remove the files
#file.remove(f)

#shut-up dplyr
suppressPackageStartupMessages(library(dplyr))

#seed
seed<-90210

#dimension and quality plots
graph_height <- 6
png_height <- 960
aspect_ratio <- 1
dpi <- 300 #retina(320)
sub_title<-'' #empty unless specified

#pointer weight variable exists
weight.exists<-FALSE

#set color palettes
coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
pretty_palette <- c("#1f77b4","#ff7f0e","#2ca02c", "#d62728","#9467bd","#8c564b","#e377c2")