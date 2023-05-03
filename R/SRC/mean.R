
#-----------------------------------------------------------------------------------------------

#Mean

#-----------------------------------------------------------------------------------------------

message("Mean...")

#mean_cols<-c("wl01","wl16","bo06","dv01","dv06","dv10","zw00","zw02","zw12","sc02")

df_aggr_mn<- df_weight %>%
  group_by(GEOITEM,PERIOD) %>%
  srvyr::summarize(
    bp_respons=n(),
    wl01=survey_mean(wl01,na.rm=TRUE, vartype=vt),
    wl16=survey_mean(wl16,na.rm=TRUE, vartype=vt),
    bo06=survey_mean(bo06,na.rm=TRUE, vartype=vt),
    dv01=survey_mean(dv01,na.rm=TRUE, vartype=vt),
    dv06=survey_mean(dv06,na.rm=TRUE, vartype=vt),
    dv10=survey_mean(dv10,na.rm=TRUE, vartype=vt),
    zw00=survey_mean(zw00,na.rm=TRUE, vartype=vt),
    zw02=survey_mean(zw02,na.rm=TRUE, vartype=vt),
    zw12=survey_mean(zw12,na.rm=TRUE, vartype=vt),
    sc02=survey_mean(sc02,na.rm=TRUE, vartype=vt),
    sa01=survey_mean(sa01_0,na.rm=TRUE, vartype=vt),
    sa02=survey_mean(sa01_1,na.rm=TRUE, vartype=vt),
    sa03=survey_mean(sa01_2,na.rm=TRUE, vartype=vt)
  ) %>% 
  mutate_at(.,vars(-group_cols()),~ifelse(is.nan(.) | is.infinite(.), NA, .)) %>%
  mutate_at(.,vars(-group_cols()),~replace(., .<1, NA))  

#df_aggr_mn<- df_weight %>%
#group_by(GEOITEM,PERIOD) %>%
#srvyr::summarize(
#bp_respons=n(),
#across(
#.cols =mean_cols,
#.fns  =survey_mean(na.rm=TRUE),
#.names="{col}_"
#)
#) %>% 
#mutate_at(.,vars(-group_cols()),~ifelse(is.nan(.) | is.infinite(.), NA, .)) %>%
#mutate_at(.,vars(-group_cols()),~replace(., .== 0, NA)) 