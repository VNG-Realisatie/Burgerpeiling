
#----------------------------------------------------------------------------------------------

#Plotting wellbeing by typology

#----------------------------------------------------------------------------------------------

#under construction
df_ss_type <- df_ss %>%
  select(typology, ses_ss, zw00,kunnen_ss, sv_ss, vangnet_ss, socrel_ss, alone_ss, bereid_ss, part_score) %>%
  #filter(GEOITEM=867) %>% #waalwijk
  group_by(typology) %>%
  summarise_each(funs( mean( .,na.rm = TRUE)))

typol_levels<-c(1,2,3,4)
typol_labels<-c("weerbaren","buitenstaanders","compenseerders","kwetsbaren")
typol_axis<-c("ses","subjectieve gezondheid", "eigen kracht", "sociale veerkracht", "sociaal vangnet",
              "kwaliteit sociale relaties", "eenzaamheid", "bereidheid inzet mtsch. participatie" , "inzet mtsch.participatie"  ) 

df_ss_type$typology<- factor(df_ss_type$typology, levels=typol_levels, labels=typol_labels)

devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)
library(ggradar)

wellbeing<-ggradar(df_ss_type,
        axis.labels = typol_axis,
        base.size = 10,
        fill = TRUE,
        fill.alpha = 0.1,
        grid.min = 0,
        grid.mid = 5,
        grid.max = 10, values.radar = c(0,5,10),
        axis.label.size = 3,
        legend.text.size = 8,
        legend.position = "bottom",
        group.line.width = 0.5,
        group.point.size = 4,
        ) 
wellbeing
plot.nme = paste0('wellbeing_typology.png')
plot.store <-paste0(plots.dir,'/',plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * 1.5, dpi=dpi)


