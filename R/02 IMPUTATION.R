
#-----------------------------------------------------------------------------------------------

# Imputation scale scores well-being and vitality 

#-----------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------

# Setup environment

#-----------------------------------------------------------------------------------------------

#Run in project environment (to avoid package conflicts)
proj_env<-FALSE #default (F)

#packages
source('SRC/packages.R')

source(here::here('SRC/globals.R'))

#decimal notation: overrule settings in globals.R
options(OutDec=".")

#-----------------------------------------------------------------------------------------------

# READ DATA AND SUBSETTING

#-----------------------------------------------------------------------------------------------

#load rdata
resp.df<-paste0(output.dir,"/BP_respond.RData")
load(resp.df)

#clear Haven data labelling 
df_respond <-  as.tibble(sapply(df_respond, haven::zap_labels)) 

#keep track of respondent id
response_id<-df_respond$id

# GEOITEM, well-being and vitality scale scores
vars_som<-c(
  #well-being (person)
  "ses_ss","socrel_ss","zw02","zw00","kunnen_ss","bereid_ss","par_ss","vangnet_ss", "sv_ss",
  
  #vitality (#environment/community)
  "wl01","verbonden_ss", "samenredzaam_ss","safe_ss","fk_ss", "voorzieningen_ss","betrouwbaar_ss",
  
  "welzijn_ss","vitaliteit_ss"
)

#number of scale score variables
ss_len<-length(vars_som)
ss_len

#filter by municipality ids
#all municipalities in data frame
munics<-unique(df_respond$GEOITEM)
#or, subset
#munics<-c(147,384)

#number of municipalities
munics_len<-length(munics)

#scale scores set for SOM (Self-Organising Maps)
df_respond <- df_respond %>%
  #relevant scale scores
  select(all_of(c(vars_som, "GEOITEM"))) %>% 
  #all numeric
  mutate_all(., function(x) as.numeric(as.character(x))) %>%
  #filter by municipality id
  filter(GEOITEM %in% munics)

#remove GEOITEM, ... etc.(keep scale scores)
#df_scores$GEOITEM<-NULL
df_scores<-df_respond[,!names(df_respond) %in% c("GEOITEM")]


#-----------------------------------------------------------------------------------------------

# MISSING VALUES AND IMPUTATION

#-----------------------------------------------------------------------------------------------

#stats on missing values (pre-imputation)
sapply(df_scores, function(x) sum(is.na(x)))

#plot missing data pattern
#md.pattern(df_scores,plot = T)

#missing data imputation
#method : Multivariate Imputation via Chained Equations (MICE), Predictive mean matching (pmm)
#More info : https://stefvanbuuren.name/mice/

#see methods available
#methods(mice)

#initial run to auto-determine powerful predictors for imputation, tweak correlation value accordingly
ini <- mice(df_scores,pred=quickpred(df_scores, mincor=.35),seed=seed, print=F)
#predictor matrix
(pred <- ini$pred)
#save prediction matrix
#save(pred, file = "prediction_matrix_features.RData")

#or load a predefined prediction matrix
#pred <- load("prediction_matrix_features.RData")

#first run (low number of iterations)
#method: Predictive mean matching (pmm)
imp_data <- mice(df_scores[,!names(df_scores) %in% c("GEOITEM")],method = "pmm", 
                 pred=pred,m=5,maxit=10,seed=500, print=T)
summary(imp_data)

#convergence
#The mice function implements an iterative Markov Chain Monte Carlo type of algorithm.
#it is important that convergence takes effect towards the end of the iteration process
plot.nme = paste0('convergence_imputation_iterations_first_run.png')
plot.store <-paste0(plots.dir,'/',plot.nme)
png(filename=plot.store,height = png_height,width = png_height)
cplot_1 <- plot(imp_data)
cplot_1
dev.off()

#second run
#do additional iterations lead to better convergence than in first run?
imp_ext <- mice.mids(imp_data, maxit=15, print=F)

plot.nme = paste0('convergence_imputation_iterations_second_run.png')
plot.store <-paste0(plots.dir,'/',plot.nme)
png(filename=plot.store,height = png_height,width = png_height)
cplot_2 <- plot(imp_ext)
cplot_2
dev.off()

#if so, use the extended version (otherwize adjust maxit in mice.mids)
imp_data <- imp_ext

#densityplot : imputation versus oberservation (proportions and distribution are important here)
plot.nme = paste0('imputation_pattern.png')
plot.store <-paste0(plots.dir,'/',plot.nme)
png(filename=plot.store,height = png_height,width = png_height * 3)
dplot <- densityplot(imp_data)
dplot
dev.off()

#apply imputated values to SOURCE_SUBSET
df_scores <- complete(imp_data)

#stats on missing values (post-imputation). All gone!
sapply(df_scores, function(x) sum(is.na(x)))

#store imputed scale score data
scores.df<-paste0(output.dir,"/BP_SS_IMPUTE.RData")
save(df_scores, file = scores.df)
