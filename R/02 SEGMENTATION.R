
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
png(filename=plot.store,height = png_height,width = png_height * 3)
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


#-----------------------------------------------------------------------------------------------

# SEGMENTATION ROUTE I: SOM HCLUS KMEANS

#-----------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------

# Self-Organising Maps 

#-----------------------------------------------------------------------------------------------

#Self-Organising Maps (SOMs), or Kohonen maps, is  an unsupervised data visualisation technique that 
#can be used to visualise high-dimensional data sets in lower (typically 2) dimensional representations.
#SOM maintains a topology embedded in the data space (as opposed to Kmeans)
#SOMs are a type of artificial neural network (ANN) that are trained using unsupervised learning

#convert training data to a matrix and center and scale all variables to give them equal importance 
#during SOM training process. 
data_train_matrix <- as.matrix(scale(df_scores))

#create the SOM grid
som_grid <- kohonen::somgrid(xdim = 20, ydim=20, topo="hexagonal")

#train the SOM, options for the number of iterations,the learning rates, and the neighbourhood are available
som_model <- kohonen::som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=700, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE )

#training progress for SOM
som.nme<-paste0(plots.dir,'/som_changes.png')
png(file=som.nme, width = 960, height = 960, units = "px")
plot(som_model, type="changes")
dev.off()

#node count plot
som.nme<-paste0(plots.dir,'/som_nodes.png')
png(file=som.nme, width = 960, height = 960, units = "px")
plot(som_model, type="count", main="Node Counts")
dev.off()

#U-matrix visualization
som.nme<-paste0(plots.dir,'/som_distance.png')
png(file=som.nme, width = 960, height = 960, units = "px")
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances")
dev.off()

#Weight Vector View
som.nme<-paste0(plots.dir,'/som_codes.png')
png(file=som.nme, width = 960, height = 960, units = "px")
plot(som_model, type="codes")
dev.off()

#-----------------------------------------------------------------------------------------------


#plot Kohonen heatmap for each feature i.c. scale score
for(i in 1:ss_len) {
# Kohonen Heatmap creation
khm.nme<-paste0(plots.dir,'/som_heatmap_',i,'.png')
png(file=khm.nme, width = 960, height = 960, units = "px")
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
#use hierarchical clustering to assign each neuron to a cluster.
som_cluster <- cutree(hclust(dist(som_data)), k)

#-----------------------------------------------------------------------------------------------

# CLUSTER ASSIGNMNENT

#-----------------------------------------------------------------------------------------------

clus.nme<-paste0(plots.dir,'/som_clusters.png')
png(file=clus.nme)
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model, som_cluster)
dev.off()

# get vector with cluster value for each original data sample
cluster_assignment <- som_cluster[som_model$unit.classif]

df_scores$cluster_assignment <- cluster_assignment
df_respond$cluster_assignment <- cluster_assignment


#-----------------------------------------------------------------------------------------------

# CHARACTERISTICS OF CLUSTERS

#-----------------------------------------------------------------------------------------------

#distribution of cluster assignment
plot.nme<-paste0(plots.dir,'/clusters_assignment_distribution.png')
png(file=plot.nme,height = png_height,width = png_height * 2)
ggplot(df_scores) +
  geom_histogram(aes(x = cluster_assignment,
                 y = after_stat(density)),
                 bins = k, fill = "#112446"
                 , color="white") +
  stat_bin(
    aes(x = cluster_assignment,
        y = after_stat(density),
        label = after_stat(ifelse(count == 0, "", count))),
    bins = k, geom = "text", vjust = -1
  ) +
  labs(
    x = "cluster",
    y = "n",
    title = "Incidentie naar cluster"
  ) +
  theme_minimal()
dev.off()

#vitaliteit by cluster (colour represents perceived health)
plot.nme<-paste0(plots.dir,'/clusters_vitaliteit_gezondheid.png')
png(file=plot.nme,height = png_height,width = png_height * 2)
ggplot(df_scores) +
 aes(x = cluster_assignment, y = vitaliteit_ss, colour = zw02) +
 geom_point(shape = "circle", 
 size = 1.5) +
  geom_jitter() +
  scale_color_viridis(discrete = FALSE) +
  labs(
    x = "cluster",
    y = "vitaliteit (schaalscore)",
    color = "ervaren gezondheid"
  ) +
 theme_minimal()
dev.off()


plot.nme<-paste0(plots.dir,'/clusters_vitaliteit_socialeveerkracht.png')
png(file=plot.nme,height = png_height,width = png_height * 2)

ggplot(df_scores) +
  aes(x = cluster_assignment, y = vitaliteit_ss, colour = sv_ss) +
  geom_point(shape = "circle", 
             size = 1.5) +
  geom_jitter() +
  scale_color_viridis(discrete = FALSE) +
  labs(
    x = "cluster",
    y = "vitaliteit (schaalscore)",
    color = "sociale veerkracht"
  ) +
  theme_minimal()
dev.off()

plot.nme<-paste0(plots.dir,'/clusters_welzijn_sess.png')
png(file=plot.nme,height = png_height,width = png_height * 2)

ggplot(df_scores) +
  aes(x = cluster_assignment, y = welzijn_ss, colour = ses_ss) +
  geom_point(shape = "circle", 
             size = 1.5) +
  geom_jitter() +
  scale_color_viridis(discrete = FALSE) +
  labs(
    x = "cluster",
    y = "welzijn (schaalscore)",
    color = "SES"
  ) +
  theme_minimal()
dev.off()


#-----------------------------------------------------------------------------------------------

# CLUSTERS VS TYPOLOGY

#-----------------------------------------------------------------------------------------------

corrs<-df_respond %>%
  select(cluster_assignment,typology) %>%
  mutate_all(., function(x) (as.numeric(as.character(x))))

#print(cor(corrs$cluster_assignment, corrs$typology, method = "pearson")) 


#END OF SOM


#-----------------------------------------------------------------------------------------------

# ROUTE II UMAP KMEANS

#-----------------------------------------------------------------------------------------------

#.....
