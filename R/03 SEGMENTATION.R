
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

#load imputed scale scores rdata
resp.df<-paste0(output.dir,"/BP_SS_IMPUTE.RData")
load(resp.df)


#-----------------------------------------------------------------------------------------------

# Self-Organising Maps 

#-----------------------------------------------------------------------------------------------

#Self-Organising Maps (SOMs), or Kohonen maps, is  an unsupervised data visualisation technique that 
#can be used to visualise high-dimensional data sets in lower (typically 2) dimensional representations.
#SOM maintains a topology embedded in the data space (as opposed to e.g. Kmeans)
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
khm.nme<-paste0(plots.dir,'/som_heatmap_',vars_som[i],'.png')
png(file=khm.nme, width = 960, height = 960, units = "px")
plot(som_model, type = "property", property = getCodes(som_model)[,i], 
     main=colnames(getCodes(som_model))[i], palette.name=coolBlueHotRed)
dev.off()
}

#-----------------------------------------------------------------------------------------------
#Kmeans

#optimal number of clusters
som_data <- getCodes(som_model)

#Calculate the Within-Cluster-Sum of Squared Errors (WSS) for different values of i (number of clusters), 
#and choose the i for which WSS becomes first starts to diminish
wss <- (nrow(som_data)-1)*sum(apply(som_data,2,var)) 

for (i in 2:15) {
  wss[i] <- sum(kmeans(som_data, centers=i)$withinss)
}
plot(wss)

#set 'optimal' number of clusters (based on i)
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

#distribution of cluster assignment (abs)
cluster_dis_abs<-ggplot(df_scores) +
  geom_histogram(aes(x = cluster_assignment,
                 y = after_stat(density)),
                 bins = k , fill = "#112446"
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
  theme_minimal() + 
  theme(legend.position = "none") 
cluster_dis_abs
plot.nme<-paste0('/clusters_assignment_distribution_abs.png')
plot.store <-paste0(plots.dir,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * 2, dpi=dpi)


#distribution of cluster assignment (rel)
plot.title = paste0('Incidentie naar cluster')
cluster_dis_rel<-ggplot(df_scores, aes(factor(cluster_assignment), fill=factor(cluster_assignment))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#112446"
           , color="white") + 
  scale_y_continuous(labels=scales::percent) +
  labs(x = "cluster", y="(%)",title=plot.title, subtitle = paste0(''), fill="cluster_assignment" ) +
  theme_minimal() + 
  theme(legend.position = "none") 
cluster_dis_rel
plot.nme = paste0('/clusters_assignment_distribution_rel.png')
plot.store <-paste0(plots.dir,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * 2, dpi=dpi)


#-----------------------------------------------------------------------------------------------

## VITALITY NEIGHBOURHOOD

#-----------------------------------------------------------------------------------------------

#vitaliteit buurt by cluster (colour represents ses)
vit_ses<-ggplot(df_scores) +
 aes(x = cluster_assignment, y = vitaliteit_ss, colour = ses_ss) +
 geom_point(shape = "circle", 
 size = 1.5) +
  geom_jitter() +
  scale_color_viridis(discrete = FALSE) +
  labs(
    x = "cluster",
    y = "vitaliteit (schaalscore)",
    color = "ses"
  ) +
 theme_minimal()
vit_ses
plot.nme = paste0('/clusters_vitaliteit_ses.png')
plot.store <-paste0(plots.dir,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * 2, dpi=dpi)


#vitaliteit buurt by cluster (colour represents cohesion)
vit_coh<-ggplot(df_scores) +
  aes(x = cluster_assignment, y = vitaliteit_ss, colour = verbonden_ss) +
  geom_point(shape = "circle", 
             size = 1.5) +
  geom_jitter() +
  scale_color_viridis(discrete = FALSE) +
  labs(
    x = "cluster",
    y = "vitaliteit (schaalscore)",
    color = "sociale samenhang"
  ) +
  theme_minimal()
vit_coh
plot.nme = paste0('/clusters_vitaliteit_socialesamenhang.png')
plot.store <-paste0(plots.dir,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * 2, dpi=dpi)


#vitaliteit buurt by cluster (colour represents participation)
vit_par<-ggplot(df_scores) +
  aes(x = cluster_assignment, y = vitaliteit_ss, colour = par_ss) +
  geom_point(shape = "circle", 
             size = 1.5) +
  geom_jitter() +
  scale_color_viridis(discrete = FALSE) +
  labs(
    x = "cluster",
    y = "vitaliteit buurt (schaalscore)",
    color = "mtsch. participatie"
  ) +
  theme_minimal()
vit_par
plot.nme = paste0('/clusters_vitaliteit_mtschparticipatie.png')
plot.store <-paste0(plots.dir,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * 2, dpi=dpi)


#-----------------------------------------------------------------------------------------------

## PERSONAL WELL-BEING

#-----------------------------------------------------------------------------------------------

#personal well-being by cluster (colour represents ses)
well_ses<-ggplot(df_scores) +
  aes(x = cluster_assignment, y = welzijn_ss, colour = ses_ss) +
  geom_point(shape = "circle", 
             size = 1.5) +
  geom_jitter() +
  scale_color_viridis(discrete = FALSE) +
  labs(
    x = "cluster",
    y = "persoonlijk welzijn (schaalscore)",
    color = "SES"
  ) +
  theme_minimal()
well_ses
plot.nme = paste0('/clusters_welzijn_ses.png')
plot.store <-paste0(plots.dir,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * 2, dpi=dpi)


#personal well-being by cluster (colour represents social resilience)
well_sv<-ggplot(df_scores) +
  aes(x = cluster_assignment, y = welzijn_ss, colour = sv_ss) +
  geom_point(shape = "circle", 
             size = 1.5) +
  geom_jitter() +
  scale_color_viridis(discrete = FALSE) +
  labs(
    x = "cluster",
    y = "persoonlijk welzijn (schaalscore)",
    color = "sociale veerkracht"
  ) +
  theme_minimal()
well_sv
plot.nme = paste0('/clusters_welzijn_socveerkracht.png')
plot.store <-paste0(plots.dir,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * 2, dpi=dpi)


#personal well-being by cluster (colour represents participation)
well_par<-ggplot(df_scores) +
  aes(x = cluster_assignment, y = welzijn_ss, colour = sv_ss) +
  geom_point(shape = "circle", 
             size = 1.5) +
  geom_jitter() +
  scale_color_viridis(discrete = FALSE) +
  labs(
    x = "cluster",
    y = "persoonlijk welzijn (schaalscore)",
    color = "mtsch. participatie"
  ) +
  theme_minimal()
well_par
plot.nme = paste0('/clusters_welzijn_mtschparticipatie.png')
plot.store <-paste0(plots.dir,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * 2, dpi=dpi)

#-----------------------------------------------------------------------------------------------

#Dispersion of vitality
plot.title = paste0('Vitaliteit buurt naar cluster')
dp1<-ggplot(df_scores, aes(x = factor(cluster_assignment), y =vitaliteit_ss,fill=factor(cluster_assignment))) + 
  geom_boxplot(width=0.6) + 
  stat_summary(fun=mean, geom="point", shape=5, size=4) +
  scale_fill_viridis_d() +
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title=plot.title, subtitle=paste0(''), fill="cluster_assignment" ) +
  xlab("cluster") +
  ylab("vitaliteit buurt (schaalscore)")
dp1
plot.nme = paste0('/clusters_vitaliteit_dispersion.png')
plot.store <-paste0(plots.dir,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * aspect_ratio, dpi=dpi)


#Dispersion of well-being
plot.title = paste0('Persoonlijk welzijn naar cluster')
dp2<-ggplot(df_scores, aes(x = factor(cluster_assignment), y =welzijn_ss,fill=factor(cluster_assignment))) + 
  geom_boxplot(width=0.6) + 
  stat_summary(fun=mean, geom="point", shape=5, size=4) +
  scale_fill_viridis_d() +
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title=plot.title, subtitle=paste0(''), fill="cluster_assignment" ) +
  xlab("cluster") +
  ylab("persoonlijk welzijn (schaalscore)")
dp2
plot.nme = paste0('/clusters_welzijn_dispersion.png')
plot.store <-paste0(plots.dir,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * aspect_ratio, dpi=dpi)


#Dispersion of social resilience
plot.title = paste0('Sociale veerkracht naar cluster')
dp3<-ggplot(df_scores, aes(x = factor(cluster_assignment), y =sv_ss,fill=factor(cluster_assignment))) + 
  geom_boxplot(width=0.6) + 
  stat_summary(fun=mean, geom="point", shape=5, size=4) +
  scale_fill_viridis_d() +
  theme_minimal() + 
  theme(legend.position = "none") +
  labs(title=plot.title, subtitle=paste0(''), fill="cluster_assignment" ) +
  xlab("cluster") +
  ylab("sociale veerkracht (schaalscore)")
dp3
plot.nme = paste0('/clusters_socveerkracht_dispersion.png')
plot.store <-paste0(plots.dir,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * aspect_ratio, dpi=dpi)


#-----------------------------------------------------------------------------------------------

# CLUSTERS VS TYPOLOGY

#-----------------------------------------------------------------------------------------------

corrs<-df_respond %>%
  select(cluster_assignment,typology) %>%
  mutate_all(., function(x) (as.numeric(as.character(x))))

#print(cor(corrs$cluster_assignment, corrs$typology, method = "pearson")) 


#END OF SOM