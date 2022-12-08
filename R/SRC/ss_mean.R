
#-----------------------------------------------------------------------------------------------

#Scale score mean

#-----------------------------------------------------------------------------------------------

df_ss_aggr<- df_ss_weight %>%
  ungroup() %>%
  group_by(GEOITEM,PERIOD) %>%
  summarize(
    part_score_mean=survey_mean(part_score,na.rm=TRUE, vartype=vt),
    qol_score_mean=survey_mean(qol_score,na.rm=TRUE, vartype=vt),
    unity_ss_mean=survey_mean(verbonden_ss,na.rm=TRUE, vartype=vt),
    ses_ss_mean=survey_mean(ses_ss,na.rm=TRUE, vartype=vt),
    samenredzaam_ss_mean=survey_mean(samenredzaam_ss,na.rm=TRUE, vartype=vt),  
    safe_ss_mean=survey_mean(safe_ss,na.rm=TRUE, vartype=vt),
    voorzieningen_ss_mean=survey_mean(voorzieningen_ss,na.rm=TRUE, vartype=vt),
    fk_ss_mean=survey_mean(fk_ss,na.rm=TRUE, vartype=vt),
    par_ss_mean=survey_mean(par_ss,na.rm=TRUE, vartype=vt),
    trust_ss_mean=survey_mean(trust_ss,na.rm=TRUE, vartype=vt),
    betrouwbaar_ss_mean=survey_mean(betrouwbaar_ss,na.rm=TRUE, vartype=vt),
    kunnen_ss_mean=survey_mean(kunnen_ss,na.rm=TRUE, vartype=vt),
    sv_ss_mean=survey_mean(sv_ss,na.rm=TRUE, vartype=vt),
    bereid_ss_mean=survey_mean(bereid_ss,na.rm=TRUE, vartype=vt),
    socrel_ss_mean=survey_mean(socrel_ss,na.rm=TRUE, vartype=vt),
    alone_ss_mean=survey_mean(alone_ss,na.rm=TRUE, vartype=vt),
    vangnet_ss_mean=survey_mean(vangnet_ss,na.rm=TRUE, vartype=vt),
    dv01_red0_mean=survey_mean(dv01_red0,na.rm=TRUE, vartype=vt),
    dv01_red1_mean=survey_mean(dv01_red1,na.rm=TRUE, vartype=vt),
    dv06_red0_mean=survey_mean(dv06_red0,na.rm=TRUE, vartype=vt),
    dv06_red1_mean=survey_mean(dv06_red1,na.rm=TRUE, vartype=vt),
    zw12_red0_mean=survey_mean(zw12_red0,na.rm=TRUE, vartype=vt),
    zw12_red1_mean=survey_mean(zw12_red1,na.rm=TRUE, vartype=vt),
    hostman_ss_red0_mean=survey_mean(hostman_ss_red0,na.rm=TRUE, vartype=vt),
    hostman_ss_red1_mean=survey_mean(hostman_ss_red1,na.rm=TRUE, vartype=vt),
    vitaliteitwelzijn_ss=survey_mean(vitaliteitwelzijn_ss,na.rm=TRUE, vartype=vt),
    vitaliteit_ss=survey_mean(vitaliteit_ss,na.rm=TRUE, vartype=vt),
    welzijn_ss=survey_mean(welzijn_ss,na.rm=TRUE, vartype=vt),
  ) %>%  
  mutate_at(.,vars(-group_cols()),~ifelse(is.nan(.) | is.infinite(.), NA, .)) %>%
  mutate_at(.,vars(-group_cols()),~replace(., .== 0, NA))  

#	hostman_pin1	

#_______________________________________________________________________  
#MEAN scale scores (gemeente, jaar, typologie)

df_ss_type_aggr<- df_ss_weight %>%
  ungroup() %>%
  filter(!is.na(typology)) %>%
  group_by(GEOITEM,PERIOD, typology) %>%
  summarize(
    part_score_mean=survey_mean(part_score,na.rm=TRUE, vartype=vt),
    qol_score_mean=survey_mean(qol_score,na.rm=TRUE, vartype=vt),
    unity_ss_mean=survey_mean(verbonden_ss,na.rm=TRUE, vartype=vt),
    ses_ss_mean=survey_mean(ses_ss,na.rm=TRUE, vartype=vt),
    samenredzaam_ss_mean=survey_mean(samenredzaam_ss,na.rm=TRUE, vartype=vt),  
    safe_ss_mean=survey_mean(safe_ss,na.rm=TRUE, vartype=vt),
    voorzieningen_ss_mean=survey_mean(voorzieningen_ss,na.rm=TRUE, vartype=vt),
    fk_ss_mean=survey_mean(fk_ss,na.rm=TRUE, vartype=vt),
    par_ss_mean=survey_mean(par_ss,na.rm=TRUE, vartype=vt),
    trust_ss_mean=survey_mean(trust_ss,na.rm=TRUE, vartype=vt),
    betrouwbaar_ss_mean=survey_mean(betrouwbaar_ss,na.rm=TRUE, vartype=vt),
    kunnen_ss_mean=survey_mean(kunnen_ss,na.rm=TRUE, vartype=vt),
    sv_ss_mean=survey_mean(sv_ss,na.rm=TRUE, vartype=vt),
    bereid_ss_mean=survey_mean(bereid_ss,na.rm=TRUE, vartype=vt),
    socrel_ss_mean=survey_mean(socrel_ss,na.rm=TRUE, vartype=vt),
    alone_ss_mean=survey_mean(alone_ss,na.rm=TRUE, vartype=vt),
    vangnet_ss_mean=survey_mean(vangnet_ss,na.rm=TRUE, vartype=vt),
    vitaliteitwelzijn_ss=survey_mean(vitaliteitwelzijn_ss,na.rm=TRUE, vartype=vt),
    #vitaliteit_ss=survey_mean(vitaliteit_ss,na.rm=TRUE, vartype=vt),
    #welzijn_ss=survey_mean(welzijn_ss,na.rm=TRUE, vartype=vt),
    gezond_mean=survey_mean(zw02,na.rm=TRUE, vartype=vt),
    geluk_mean=survey_mean(zw00,na.rm=TRUE, vartype=vt),
    wl01_mean=survey_mean(wl01,na.rm=TRUE, vartype=vt),
    gelukkig_mean=survey_mean(gelukkig,na.rm=TRUE, vartype=vt)*100
  ) %>%
  pivot_wider(names_from=typology, values_from=-names(.)[1:3],names_sep="b") %>%
  mutate_at(.,vars(-group_cols()),~ifelse(is.nan(.) | is.infinite(.), NA, .)) %>%
  mutate_at(.,vars(-group_cols()),~replace(., .== 0, NA))  


#_______________________________________________________________________  
#MEAN scale scores (gemeente, jaar, leeftijd)

df_ss_age_aggr<- df_ss_weight %>%
  ungroup() %>%
  filter(!is.na(lft_cy)) %>%
  group_by(GEOITEM,PERIOD,lft_cy) %>%
  summarize(
    kunnen=survey_mean(kunnen_ss,na.rm=TRUE, vartype=vt),
    socrel=survey_mean(socrel_ss,na.rm=TRUE, vartype=vt),
    par=survey_mean(par_ss,na.rm=TRUE, vartype=vt),
    bereid=survey_mean(bereid_ss,na.rm=TRUE, vartype=vt),
    safe=survey_mean(safe_ss,na.rm=TRUE, vartype=vt),
    ses=survey_mean(ses_ss,na.rm=TRUE, vartype=vt),
    sv=survey_mean(sv_ss,na.rm=TRUE, vartype=vt),
    #ver=survey_mean(zw05_rc,na.rm=TRUE, vartype=vt),
    #mz=survey_mean(zw06_0,na.rm=TRUE, vartype=vt),
    #bh=survey_mean(zw06_1,na.rm=TRUE, vartype=vt),
    #vw=survey_mean(zw06_3,na.rm=TRUE, vartype=vt),
    gelukkig_mean=survey_mean(gelukkig,na.rm=TRUE, vartype=vt)*100,
    health=survey_mean(zw02,na.rm=TRUE, vartype=vt),
    happy=survey_mean(zw00,na.rm=TRUE, vartype=vt)
  ) %>%
  pivot_wider(names_from=lft_cy, values_from=-names(.)[1:3],names_sep="_cy")  %>%
  mutate_at(.,vars(-group_cols()),~ifelse(is.nan(.) | is.infinite(.), NA, .)) %>%
  mutate_at(.,vars(-group_cols()),~replace(., .== 0, NA))  
