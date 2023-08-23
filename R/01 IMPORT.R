
#-----------------------------------------------------------------------------------------------

# Burgerpeiling Waar Staat Je Gemeente?

#-----------------------------------------------------------------------------------------------

#this procedure is to check the results of the Burgerpeiling as (will be) presented on
#Waarstaatjegemeente.nl (VNG). Moreover it enables you to generate additional indicators and
#visualizations

#this procedure is not intended to prepare data for publishing on Waarstaatjegemeente.nl

#see 'Beschrijving' directory for specification of the variables.

#last update 2023-08-23 (alpha version)

#questions? contact Mark Henry Gremmen mark.gremmen@vng.nl (VNG)

#-----------------------------------------------------------------------------------------------

# Setup environment

#-----------------------------------------------------------------------------------------------

#Run in an isolated project environment (to avoid package conflicts)
proj_env<-FALSE #default (F)

# Load necessary libraries
source('SRC/packages.R')

# Load global variables
source(here::here('SRC/globals.R'))

#-----------------------------------------------------------------------------------------------

# Set pipeline timer
start_time<-Sys.time()

#-----------------------------------------------------------------------------------------------

# IMPORT 

#-----------------------------------------------------------------------------------------------

message("Import Burgerpeiling(en)...")

#read multiple Spss sav and RData-files
file_types <- c("sav", "RData")  # Specify the file types to import

files <- fs::dir_ls(path = data.dir, recurse=FALSE) %>%
  `[`(tools::file_ext(.) %in% file_types)

df <- map_df(files, function(file) {
  ext <- tools::file_ext(file)
  
  if (ext == "sav") {
    haven::read_sav(file) %>% as_tibble()
  } else if (ext == "RData") {
    data <- get(load(file))
    # Assuming the RData file contains a single data frame
    as_tibble(data)
  } else {
    NULL  # Ignore files with unsupported extensions
  }
}, .id = "file_id")  # Add a column to identify the source file


#weight available?
weight.exists<-any(colnames(df) == "weging")
if(weight.exists==FALSE) { stop("column weging does not exist!") }

#-----------------------------------------------------------------------------------------------

# VALIDATION AND PREPERATION

#-----------------------------------------------------------------------------------------------

source(here::here('SRC/preperation.R'))

#-----------------------------------------------------------------------------------------------

# SUBSETTING

#-----------------------------------------------------------------------------------------------

message("Subsetting...")

df<-df %>% 
  #municipality id exists
  filter(!is.na(gemnr)) %>%
  #municipality
  #filter(gemnr==1955) %>%
  #year
  filter(jr>=2021) %>%
  #Weight lower than 5
  filter(!is.na(weging) & weging<5) %>%
  #valid variables (see SRC>preparation.R)
  select(any_of(var_vec))

#identify numeric variables
numeric_cols<- unlist(lapply(df, is.numeric))         
numeric_cols

#select string variables
df_string<-df[,!numeric_cols]

#select numeric variables
df<-df[,numeric_cols]

ncol(df)

#-----------------------------------------------------------------------------------------------

# MISSINGS

#-----------------------------------------------------------------------------------------------

source(here::here('SRC/missing.R'))

#-----------------------------------------------------------------------------------------------

# FACTORS AND LEVELS

#-----------------------------------------------------------------------------------------------

source(here::here('SRC/factors.R'))

#-----------------------------------------------------------------------------------------------

# IDENTIFIER

#-----------------------------------------------------------------------------------------------

#set sequence number
df$seq<-row.names(df)

#set unique identifier
df$id<-paste0("BP",df$GEOITEM,"Y",df$PERIOD,"S",df$seq)

#out.file<-paste0(output.dir,"/BP-combined.RData")
#save(df, file = out.file)

#order records by id
#df<-df %>%
#  order(id)

#reporting municipalities
munic.active<-levels(factor(df$GEMEENTE))
geoitem.active<-as.numeric(levels(factor(df$GEOITEM)))

cat("reporting municipalities: ", munic.active)

#-----------------------------------------------------------------------------------------------

#isolate from merged file
#geoitem.sep<-geoitem.active

#df_clean<- df %>%
#  filter(!GEOITEM %in% geoitem.sep)

#write_sav(df_clean, "BP-unique2122.sav") 

#-----------------------------------------------------------------------------------------------

# RECODE

#-----------------------------------------------------------------------------------------------

source(here::here('SRC/recode.R'))

#-----------------------------------------------------------------------------------------------

# Multiple response sets

#-----------------------------------------------------------------------------------------------

source(here::here('SRC/mrsets.R'))

#-----------------------------------------------------------------------------------------------

# WEIGHT

#-----------------------------------------------------------------------------------------------

message("apply Weight...")

#apply weight to df
df_weight<- df %>% 
  srvyr::as_survey_design(ids=1, # 1 for no cluster ids 
                          weights=weging, # weight added
                          strata=NULL) # sampling was simple (no strata) 
                         
#Report variability as one or more of: standard error ("se", default), 
#confidence interval ("ci"), variance ("var") or coefficient of variation ("cv").
vt<-NULL

#-----------------------------------------------------------------------------------------------

# AGGREGATE

#-----------------------------------------------------------------------------------------------

#MEAN (gemeente, jaar)
mean_cols<-c("wl01","wl16","bo06","dv01","dv06","dv10","zw00","zw02","zw12","sc02"
             #,"sa01","sa02","sa03"
)

source(here::here('SRC/mean.R'))

#-----------------------------------------------------------------------------------------------

#PERCENTAGE IN (gemeente, jaar)

source(here::here('SRC/pin.R'))

#-----------------------------------------------------------------------------------------------

#SCALE SCORES

#-----------------------------------------------------------------------------------------------

#scale score definitions
#https://www.waarstaatjegemeente.nl/Jive/ViewerReportContents.ashx?report=wsjg_bp_bijlage
 
#row wise operations
#parallel processing

message("Init parallel processing")

cluster <- multidplyr::new_cluster(parallel::detectCores() - 1)
multidplyr::cluster_library(cluster, c('tidyverse', 'furrr'))
e <- 10
multidplyr::cluster_copy(cluster, "e")
  
source(here::here('SRC/ss_recode.R'))

#-----------------------------------------------------------------------------------------------

#TYPOLOGY 

#-----------------------------------------------------------------------------------------------

source(here::here('SRC/typology.R'))

#-----------------------------------------------------------------------------------------------

#MEAN scales cores 

#-----------------------------------------------------------------------------------------------

source(here::here('SRC/ss_mean.R'))

#-----------------------------------------------------------------------------------------------

# MERGE dataframes

#-----------------------------------------------------------------------------------------------

message("Merge...")

#respondent level
df_respond<- cbind(df,df_ss)

#gemeente level

df_list<-list(df_aggr_mn,df_aggr_pin,df_ss_aggr,df_ss_type_aggr,df_ss_age_aggr,df_ss_vital)
merge_key <- c("GEOITEM","PERIOD")

# Merge data frames using reduce
merged_df <- reduce(df_list, function(x, y) merge(x, y, by = merge_key, all = TRUE))

df_munic<-cbind(merged_df,mr_sets)


#-----------------------------------------------------------------------------------------------

# WSJG 

#-----------------------------------------------------------------------------------------------

source(here::here('SRC/wsjg.R'))

#-----------------------------------------------------------------------------------------------

# EXPORT

#-----------------------------------------------------------------------------------------------

#regular export
#csv
munic.csv<-paste0(output.dir,"/BP_munic.csv")
write.table(df_munic, file=munic.csv,quote=TRUE, sep=";", dec = ",", row.names=FALSE)

#rdata gemeente
munic.df<-paste0(output.dir,"/BP_munic.RData")
save(df_munic, file = munic.df)

#rdata respondent
resp.df<-paste0(output.dir,"/BP_respond.RData")
save(df_respond, file = resp.df)

#-----------------------------------------------------------------------------------------------

#wsjg export
#csv
wsjg.csv<-paste0(output.dir,"/BP_WSJG.csv")
write.table(df_export, file=wsjg.csv,quote=TRUE, sep=";", dec = ",", row.names=FALSE)
#alternative version without quotes
wsjg.csv2<-paste0(output.dir,"/BP_WSJG_NOQUOTES.csv")
write.table(df_export, file = wsjg.csv2,quote=FALSE, sep=";", dec = ",", row.names=TRUE)

#RData
wsjg.df<-paste0(output.dir,"/BP_WSJG.RData")
save(df_export, file = wsjg.df)

#----------------------------------------------------------------------------------------------

#Plotting typology

#----------------------------------------------------------------------------------------------


#source(here::here('SRC/plotting_typology.R'))


#----------------------------------------------------------------------------------------------

#Debugging

#----------------------------------------------------------------------------------------------

end_time<-Sys.time()
process_time<-end_time - start_time

message("Finished, in ",process_time,"...")

rlang::last_error()
rlang::last_trace()