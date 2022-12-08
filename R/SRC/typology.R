
#-----------------------------------------------------------------------------------------------

#Typology

#-----------------------------------------------------------------------------------------------

#boundaries are based on all Burgerpeilingen during the last 3 years 
#optimal binning 
#qol_bin <- optbin(df_ss$qol_score, 4,na.rm = T, metric=c('mse'), max.cache=6^31)
#part_bin <- optbin(df_ss$part_score, 4,na.rm = T, metric=c('mse'), max.cache=6^31)

#hist(qol_bin)
#summary(qol_bin)

#hist(part_bin)
#summary(part_bin)

#access to threshold
#qol_bin[["thr"]][1]


df_ss<-df_ss %>%
  mutate(
    qol_score_bin=ifelse(qol_score<=3, 1,
                         ifelse(qol_score<=5, 2,
                                ifelse(qol_score<=6, 3,
                                       ifelse(is.na(qol_score), NA, 4)))),
    part_score_bin=ifelse(part_score<=2, 1,
                          ifelse(part_score<=3, 2,
                                 ifelse(part_score<=4, 3,
                                        ifelse(is.na(part_score), NA, 4)))),
    
    typology=ifelse(part_score_bin>2 & qol_score_bin>2, 1,
                    ifelse(part_score_bin<3 & qol_score_bin>2, 2,
                           ifelse(part_score_bin>2 & qol_score_bin<3 , 3,
                                  ifelse(part_score_bin<3 & qol_score_bin<3 , 4,
                                         ifelse(is.na(part_score_bin), NA, NA))))),
    
    zorwekkend=ifelse(part_score_bin<2 & qol_score_bin<2, 1,
                      ifelse(is.na(part_score_bin) | is.na(qol_score_bin) , NA, 0)),
    zelfredzaam=ifelse(typology>2, 0,
                       ifelse(typology>0, 1,
                              ifelse(is.na(part_score), NA, NA))),
    
    dv01_red0=ifelse(zelfredzaam==0, dv01,
                     ifelse(is.na(zelfredzaam), NA, NA)),
    
    
    dv01_red1=ifelse(zelfredzaam==1, dv01,
                     ifelse(is.na(zelfredzaam), NA, NA)),
    
    
    dv06_red0=ifelse(zelfredzaam==0, dv06,
                     ifelse(is.na(zelfredzaam), NA, NA)),
    
    
    dv06_red1=ifelse(zelfredzaam==1, dv06,
                     ifelse(is.na(zelfredzaam), NA, NA)),
    
    
    zw12_red0=ifelse(zelfredzaam==0, zw12,
                     ifelse(is.na(zelfredzaam), NA, NA)),
    
    
    zw12_red1=ifelse(zelfredzaam==1, zw12,
                     ifelse(is.na(zelfredzaam), NA, NA)),
    
    hostman_ss_red0=ifelse(zelfredzaam==0, hostman_ss,
                           ifelse(is.na(zelfredzaam), NA, NA)),
    
    
    hostman_ss_red1=ifelse(zelfredzaam==1, hostman_ss,
                           ifelse(is.na(zelfredzaam), NA, NA)),
    
    gelukkig=ifelse(zw00>6, 1,
                    ifelse(is.na(zw00), NA, 0))
  )


df_ss_weight<- df_ss %>% 
  srvyr::as_survey_design(ids=1, # 1 for no cluster ids 
                          weights=weging, # weight added
                          strata=NULL) # sampling was simple (no strata)

#typolody weerbaarheid
df_ss_vital<-df_ss_weight %>%
  group_by(GEOITEM,PERIOD) %>%
  summarize(
    typology_pin1=(survey_mean((typology==1), na.rm=T, vartype=vt) *100),
    typology_pin2=(survey_mean((typology==2), na.rm=T, vartype=vt) *100),
    typology_pin3=(survey_mean((typology==3), na.rm=T, vartype=vt) *100),
    typology_pin4=(survey_mean((typology==4), na.rm=T, vartype=vt) *100),
    zorgwekkend_pin1=(survey_mean((zorwekkend==1), na.rm=T, vartype=vt) *100)   
  ) %>%
  mutate_at(.,vars(-group_cols()),~replace(., .== 0, NA))  