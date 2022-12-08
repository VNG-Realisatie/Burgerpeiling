
#-----------------------------------------------------------------------------------------------

# Burgerpeiling Waar Staat Je Gemeente?

#-----------------------------------------------------------------------------------------------

#this procedure is to check the results of the Burgerpeiling as (will be) presented on
#Waarstaatjegemeente.nl (VNG), as well as to generate additional visualisations

#see'Beschrijving' directory for specification of the variables.

#last update 2022-12-12 (alpha version)

#questions? contact Mark Henry Gremmen mark.gremmen@vng.nl

#-----------------------------------------------------------------------------------------------

# Setup environment

#-----------------------------------------------------------------------------------------------

#Run in project environment (to avoid package conflicts)
proj_env<-FALSE #default (F)

#packages
source('SRC/packages.R')

source(here::here('SRC/globals.R'))

#-----------------------------------------------------------------------------------------------

#pipeline timer
start_time<-Sys.time()

#-----------------------------------------------------------------------------------------------

# IMPORT 

#-----------------------------------------------------------------------------------------------

message("Import Burgerpeiling(en)...")

#read multiple Spss sav-files
file_type<-'sav'

qry<-paste0("*",file_type)
files<- fs::dir_ls(glob=qry, path=data.dir)

df<- map_df(set_names(files), function(file) {
  file %>% 
    map_df(
      ~ haven::read_sav(file) %>% as_tibble()
    ) 
})

#weight available?
weight.exists<-any(colnames(df) == "weging")
if(weight.exists==FALSE) { stop("no weight available!") }

#-----------------------------------------------------------------------------------------------

# VALIDATION AND PREPERATION

#-----------------------------------------------------------------------------------------------

message("Validation and preperation...")

source(here::here('SRC/preperation.R'))

#-----------------------------------------------------------------------------------------------

# SUBSETTING

#-----------------------------------------------------------------------------------------------

message("Subsetting...")

df<-df %>% 
  #Weight lower than 5
  filter(weging<5) %>%
  #year
  filter(jr>2019)

#vector with valid variables
load(var.loc)
var_vec<-as.vector(var_df)

#select valid variables
df<-df %>%
  select(any_of((var_vec[["value"]])))

#identify numeric variables
numeric_cols<- unlist(lapply(df, is.numeric))         
numeric_cols

#select numeric variables
df<-df[,numeric_cols]

ncol(df)

#-----------------------------------------------------------------------------------------------

# MISSINGS

#-----------------------------------------------------------------------------------------------

message("Missing values...")

#missing values analysis
colSums(is.na(df))

source(here::here('SRC/missing.R'))

#-----------------------------------------------------------------------------------------------

# FACTORS AND LEVELS

#-----------------------------------------------------------------------------------------------

message("Factors and levels...")

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

# RECODE

#-----------------------------------------------------------------------------------------------

message("Recode...")

source(here::here('SRC/recode.R'))

#-----------------------------------------------------------------------------------------------

# Multiple response sets

#-----------------------------------------------------------------------------------------------

message("Multiple response sets...")

source(here::here('SRC/mrsets.R'))

#-----------------------------------------------------------------------------------------------

# WEIGHT

#-----------------------------------------------------------------------------------------------

message("Weight...")

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

message("Mean...")

source(here::here('SRC/mean.R'))

#-----------------------------------------------------------------------------------------------
#PIN (gemeente, jaar)

message("Distribution...")

source(here::here('SRC/pin.R'))

#-----------------------------------------------------------------------------------------------

#SCALE SCORES

#-----------------------------------------------------------------------------------------------

message("Scale scores...")

#scalescore definitions
#https://www.waarstaatjegemeente.nl/Jive/ViewerReportContents.ashx?report=wsjg_bp_bijlage
 
#rowwise operations
#parallel processing

cluster <- multidplyr::new_cluster(parallel::detectCores() - 1)
multidplyr::cluster_library(cluster, c('tidyverse', 'furrr'))
e <- 10
multidplyr::cluster_copy(cluster, "e")
  
source(here::here('SRC/ss_recode.R'))

#-----------------------------------------------------------------------------------------------

#TYPOLOGY 

#-----------------------------------------------------------------------------------------------

message("Typology...")

source(here::here('SRC/typology.R'))

#-----------------------------------------------------------------------------------------------
#MEAN scales cores 

message("Mean...")

source(here::here('SRC/ss_mean.R'))

#-----------------------------------------------------------------------------------------------

# MERGE dataframes

#-----------------------------------------------------------------------------------------------

message("Merge...")

#respondent level
df_respond<- cbind(df,df_ss)

#gemeente level
df_munic<- Reduce(function(x, y) merge(x, y, all=TRUE), 
                 list(df_aggr_mn,df_aggr_pin,df_rev_aggr_pin,df_ss_aggr,df_ss_type_aggr,df_ss_age_aggr,df_ss_vital))
df_munic[complete.cases(df_munic), ]

df_munic<-cbind(df_munic,mr_sets)

#df_munic <- list(df_aggr_mn,df_aggr_pin,df_ss_aggr,df_ss_type_aggr,df_ss_age_aggr,df_ss_vital) |> purrr::reduce(rbind)

#keep valid columns (remove indicators by age where age is missing)
#df_munic<-df_munic %>% select(-contains(c("_cy0", "_cyNA", ".NA")))

#-----------------------------------------------------------------------------------------------

# WSJG 

#-----------------------------------------------------------------------------------------------

message("WSJG...")

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

#rdata
wsjg.df<-paste0(output.dir,"/BP_WSJG.RData")
save(df_export, file = wsjg.df)

#----------------------------------------------------------------------------------------------

#Plotting typology

#----------------------------------------------------------------------------------------------


#source(here::here('SRC/plotting_typology.R'))


#----------------------------------------------------------------------------------------------

#Debugging

#----------------------------------------------------------------------------------------------

rlang::last_error()
rlang::last_trace()

end_time<-Sys.time()
process_time<-end_time - start_time

message("Finished, in ",process_time,"...")