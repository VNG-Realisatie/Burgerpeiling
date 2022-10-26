
#-----------------------------------------------------------------------------------------------

# Segmentation of respondents by well-being and vitality (UNDER CONSTRUCTION)

#-----------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------

# Setup environment

#-----------------------------------------------------------------------------------------------

#Run in project environment (to avoid package conflicts)
proj_env<-FALSE #default (F)

#packages
source('SRC/packages.R')

source(here::here('SRC/globals.R'))

options(OutDec=".")

#-----------------------------------------------------------------------------------------------

# READ DATA AND SUBSETTING

#-----------------------------------------------------------------------------------------------

#rdata
resp.df<-paste0(output.dir,"/BP_respond.RData")
load(resp.df)

#clear data labelling
df_respond <-  as.tibble(sapply(df_respond, haven::zap_labels)) 

#keep track of respondent id
response_id<-df_respond$id
  
# well-being and vitality scalescores
vars_som<-c("welzijn_ss","vitaliteit_ss","sv_ss","kunnen_ss","par_ss","vangnet_ss","ses_ss",
            "socrel_ss","bereid_ss", "verbonden_ss","zw00","zw02")

#number of variables
ss_len<-length(vars_som)
ss_len

#scale scores set for SOM (Self-Organising Maps)
df_scores <- df_respond %>%
  #relevant scale scores
  select(all_of(vars_som)) %>% 
  #all numeric
  mutate_all(., function(x) as.numeric(as.character(x)))
  

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
imp_data <- mice(df_scores,method = "pmm", pred=pred,m=5,maxit=10,seed=500, print=T)
summary(imp_data)

#convergence
#The mice function implements an iterative Markov Chain Monte Carlo type of algorithm.
#it is important that convergence takes effect towards the end of the iteration process
plot.nme = paste0('convergence_imputation_iterations_first_run.png')
plot.store <-paste0(plots.dir,'/',plot.nme)
png(filename=plot.store,height = png_height,width = png_height * aspect_ratio)
cplot_1 <- plot(imp_data)
cplot_1
dev.off()

#second run
#do additional iterations lead to better convergence than in first run?
imp_ext <- mice.mids(imp_data, maxit=15, print=F)

plot.nme = paste0('convergence_imputation_iterations_second_run.png')
plot.store <-paste0(plots.dir,'/',plot.nme)
png(filename=plot.store,height = png_height,width = png_height * aspect_ratio)
cplot_2 <- plot(imp_ext)
cplot_2
dev.off()

#if so, use the extended version (otherwize adjust maxit in mice.mids)
imp_data <- imp_ext

#densityplot : imputation versus oberservation (proportions and distribution are important here)
plot.nme = paste0('imputation_pattern.png')
plot.store <-paste0(plots.dir,'/',plot.nme)
png(filename=plot.store,height = png_height,width = png_height * aspect_ratio)
dplot <- densityplot(imp_data)
dplot
dev.off()

#apply imputated values to SOURCE_SUBSET
df_scores <- complete(imp_data)

#stats on missing values (post-imputation). All gone!
sapply(df_scores, function(x) sum(is.na(x)))


#-----------------------------------------------------------------------------------------------

# SEGMENTATION ROUTE I: SOM HCLUS KMEANS

#-----------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------

# Self-Organising Maps 

#-----------------------------------------------------------------------------------------------

#Self-Organising Maps (SOMs) are an unsupervised data visualisation technique that can be used to 
#visualise high-dimensional data sets in lower (typically 2) dimensional representations.


#convert training data to a matrix and center and scale all variables to give them equal importance 
#during SOM training process. 
data_train_matrix <- as.matrix(scale(df_scores))

#create the SOM grid
som_grid <- somgrid(xdim = 20, ydim=20, topo="hexagonal")

#train the SOM, options for the number of iterations,the learning rates, and the neighbourhood are available
som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=500, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE )

#training progress for SOM
plot(som_model, type="changes")

#node count plot
plot(som_model, type="count", main="Node Counts")

#U-matrix visualization
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances")

#Weight Vector View
plot(som_model, type="codes")

#-----------------------------------------------------------------------------------------------


#plot Kohonen heatmap for each scale score
for(i in 1:ss_len) {
# Kohonen Heatmap creation
khm.nme<-paste0(plots.dir,'/kohonen_heatmap_',i,'.png')
png(file=khm.nme)
plot(som_model, type = "property", property = getCodes(som_model)[,i], main=colnames(getCodes(som_model))[i], palette.name=coolBlueHotRed)
dev.off()
}

#-----------------------------------------------------------------------------------------------
#Kmeans

#optimal number of clusters
som_data <- getCodes(som_model)

#Calculate the Within-Cluster-Sum of Squared Errors (WSS) for different values of i, 
#and choose the i for which WSS becomes first starts to diminish
wss <- (nrow(som_data)-1)*sum(apply(som_data,2,var)) 

for (i in 2:15) {
  wss[i] <- sum(kmeans(som_data, centers=i)$withinss)
}
plot(wss)

#set number of clusters (based on i)
k<-8

# Visualising cluster results
## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(som_data)), k)

#-----------------------------------------------------------------------------------------------
# plot clusters

clus.nme<-paste0(plots.dir,'/som_clusters.png')
png(file=clus.nme)
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model, som_cluster)
dev.off()

# get vector with cluster value for each original data sample
cluster_assignment <- som_cluster[som_model$unit.classif]



#-----------------------------------------------------------------------------------------------

# ROUTE II UMAP KMEANS

#-----------------------------------------------------------------------------------------------

#.....
