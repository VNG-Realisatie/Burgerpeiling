
#-----------------------------------------------------------------------------------------------

#scale score recode

#-----------------------------------------------------------------------------------------------

df_ss<-  df %>% 
  rowwise(id) %>% 
  multidplyr::partition(cluster) %>% 
  mutate(
    #actief in de buurt
    act_neigh=ifelse(wl13==1, 1,
                     ifelse(is.na(wl13), NA, 0)),
    #actief in de mantelzorg
    act_mz=ifelse(zw06_0==1, 1,
                  ifelse(zw06_0==2, 1,
                         ifelse(is.na(zw06_0), NA, 0))),
    #actief in hulp aan buren
    act_hlp=ifelse(zw06_1==1, 1,
                   ifelse(zw06_1==2, 1,
                          ifelse(is.na(zw06_1), NA, 0))),
    #actief in aandacht zorgwekkende situatie
    act_zw=ifelse(zw06_2==1, 1,
                  ifelse(zw06_2==2, 1,
                         ifelse(is.na(zw06_2), NA, 0))),
    #vrijwilligerswerk
    act_vw=ifelse(zw06_3==1, 1,
                  ifelse(zw06_3==2, 1,
                         ifelse(is.na(zw06_3), NA, 0))),
    #verenigingsleven
    act_ver=ifelse(zw05_4==1, 0,
                   ifelse(is.na(zw05_4), NA, 1)),
    #opleiding
    act_opl=ifelse(ch04==8, 1,
                   ifelse(is.na(ch04), NA, 0)),
    #arbeid
    act_arb=ifelse(ch04<4, 1,
                   ifelse(is.na(ch04), NA, 0)),
    #actief sociale contacten / zeker voldoende sociale contacten
    act_soc=ifelse(zw03==1, 1,
                   ifelse(is.na(zw03), NA, 0)),
    #affectieve hulp en zorg
    act_ih=ifelse(act_mz==1 | act_zw==1, 1, 0),
    
    #lichamelijke gezondheid
    bep_gez=ifelse(zw01_0==3 | zw01_0==4, 1,
                   ifelse(is.na(zw01_0), NA, 0)),
    
    #fysiek functioneren
    bep_fys=ifelse(zw01_1==3 | zw01_1==4, 1,
                   ifelse(is.na(zw01_1), NA, 0)),
    
    #mentale gezondheid
    bep_men=ifelse(zw01_2==3 | zw01_2==4, 1,
                   ifelse(is.na(zw01_2), NA, 0)),
    
    #taal / cultuur
    bep_cul=ifelse(zw01_3==3 | zw01_3==4, 1,
                   ifelse(is.na(zw01_3), NA, 0)),
    
    #laag inkomen
    bep_ink=ifelse(zw01_4==3 | zw01_4==4, 1,
                   ifelse(is.na(zw01_4), NA, 0)),
    
    #uitsluiting
    bep_uit=ifelse(zw01_5==3 | zw01_5==4, 1,
                   ifelse(is.na(zw01_5), NA, 0)),
    
    #andere beperking
    bep_and=ifelse(zw01_6==3 | zw01_6==4, 1,
                   ifelse(is.na(zw01_6), NA, 0)),
    
    #mantelzorg overbelasting
    mz_ob=ifelse(zw09==4 | act_mz==1, 1,
                 ifelse(is.na(zw09), NA, 0)),
    
    #niveau sociale beperkingen
    mb_soc=sum(c(bep_men,bep_cul,bep_ink,bep_uit,mz_ob),na.rm=TRUE),
    
    #sociale beperking
    bep_soc=ifelse(mb_soc>0, 1,
                   ifelse(is.na(mb_soc), NA, 0)),
    #sociale beperking afwezig
    bep_soc_neg=ifelse(mb_soc>0, 0,
                       ifelse(is.na(mb_soc), NA, 1)),
    
    #lichamelijke beperking
    mb_fys=sum(bep_gez,bep_fys,bep_and,na.rm=TRUE),
    
    bep_lich=ifelse(mb_fys>0, 1,
                    ifelse(is.na(mb_fys), NA, 0)),
    
    #lichamelijke beperking afwezig
    bep_lich_neg=ifelse(mb_fys>0, 0,
                        ifelse(is.na(mb_fys), NA, 1)),
    
    #niveau meervoudige beperkingen
    mb=sum(bep_gez,bep_fys,bep_men,bep_cul,bep_ink,bep_uit,bep_and,mz_ob,na.rm=TRUE),
    
    #beperkingen afwezig
    mb_neg=ifelse(mb>0, 0,
                  ifelse(is.na(mb), NA, 1)),
    
    #meervoudige beperking 2 of meer
    mb_hv=ifelse(mb>1, 1,
                 ifelse(is.na(mb), NA, 0)),
    
    #gezond
    health=ifelse(zw02>=6, 1,
                  ifelse(is.na(zw02), NA, 0)),
    
    #prettig in de buurt
    pleasant_neigh=ifelse(wl01>=6, 1,
                          ifelse(is.na(wl01), NA, 0)),
    
    #vangnet familie, vrienden of buurtgenoten
    safeguard_fam=ifelse(zw10_0<3, 1,
                         ifelse(is.na(zw10_0), NA, 0)),
    
    safeguard_friend=ifelse(zw10_2<3, 1,
                            ifelse(is.na(zw10_2), NA, 0)),
    
    safeguard_neigh=ifelse(zw10_1<3, 1,
                           ifelse(is.na(zw10_1), NA, 0)),
    
    
    safeguard=ifelse(safeguard_fam==1 | safeguard_neigh==1 | safeguard_friend==1, 1,
                     ifelse(is.na(zw10_0) & is.na(zw10_1) & is.na(zw10_2), NA, 0)),
    
    #eenzaamheid
    alone=ifelse(zw04<3, 0,
                 ifelse(zw04==5, NA,
                        ifelse(is.na(zw04), NA, 1))),
    
    #eenzaamheid afwezigheid
    alone_neg=ifelse(zw04<3, 1,
                     ifelse(zw04==5, NA,
                            ifelse(is.na(zw04), NA, 0))),
    
    #sociale veeerkracht
    sv_0=ifelse(zw19_0<3, 1,
                ifelse(is.na(zw19_0), NA, 0)),
    
    sv_1=ifelse(zw19_1<3, 1,
                ifelse(is.na(zw19_1), NA, 0)),
    
    sv_2=ifelse(zw19_2<3, 1,
                ifelse(is.na(zw19_2), NA, 0)),
    
    sv_3=ifelse(zw19_3<3, 1,
                ifelse(is.na(zw19_3), NA, 0)),
    
    sv_4=ifelse(zw19_4<3, 1,
                ifelse(is.na(zw19_4), NA, 0)),
    
    #niveau sociale veerkracht
    sv=sum(sv_0,sv_1,sv_2,sv_3,sv_4,na.rm=TRUE),
    
    #veerkrachtige inwoner
    sv_pos=ifelse(sv>2, 1,
                  ifelse(is.na(sv), NA, 0)),
    #_______________________________________________________________________
    #Schaalscore sociaal-economische status
    
    #opleding
    ch03_wei=ifelse(ch03==1, 0,
                    ifelse(ch03==2, 1,
                           ifelse(ch03==3, 2,
                                  ifelse(ch03==4, 3,
                                         ifelse(ch03==5, 4,
                                                ifelse(ch03==6, 5,
                                                       ifelse(is.na(ch03), NA, NA))))))),
    
    #inkomen
    ch06_wei=ifelse(ch06==1, 0,
                    ifelse(ch06==2, 1,
                           ifelse(ch06==3, 2,
                                  ifelse(ch06==4, 3,
                                         ifelse(ch06==5, 4,
                                                ifelse(ch06==6, 5,
                                                       ifelse(is.na(ch06), NA, NA))))))),
    
    #arbeid
    ch04_wei=ifelse(ch04==1, 4,
                    ifelse(ch04==2, 3,
                           ifelse(ch04==3, 5,
                                  ifelse(ch04==4, 0,
                                         ifelse(ch04==5, 2,
                                                ifelse(ch04>5, 2,
                                                       ifelse(is.na(ch04), NA, NA))))))),
    
    #rondkomen
    zw01_4_wei=ifelse(zw01_4==1, 5,
                      ifelse(zw01_4==2, 4,
                             ifelse(zw01_4==3, 2,
                                    ifelse(zw01_4==4, 0,
                                           ifelse(is.na(zw01_4), NA, NA))))),
    
    
    ses_ss=mean(c(ch03_wei,ch06_wei,ch04_wei,zw01_4_wei),na.rm=TRUE) * (6/3),
    
    
    #_______________________________________________________________________
    #Schaalscore bereidheid inzet
    
    #inzet voor leefbaarheid en veiligheid buurt toekomst
    wl14_wei=ifelse(wl14==1, 4,
                    ifelse(wl14==2, 2,
                           ifelse(wl14==3, 0,
                                  ifelse(is.na(wl14), NA, NA)))),
    
    #inzet voor vrijwilligerswerk toekomst
    zw07_wei=ifelse(zw07==1, 4,
                    ifelse(zw07==2, 2,
                           ifelse(zw07==3, 0,
                                  ifelse(is.na(zw07), NA, NA)))),
    
    #inzet voor burenhulp
    hlp_wei=ifelse(zw13_9==1 | zw13_10==1, 0,
                   ifelse(act_hlp==0, 2,
                          ifelse(act_hlp>0, 4,
                                 ifelse(is.na(act_hlp), NA, NA)))),
    
    bereid_ss=mean(c(wl14_wei,zw07_wei, hlp_wei), na.rm=TRUE) * (5/2),
    
    
    #_______________________________________________________________________
    #Schaalscore samenredzaamheid buurt
    
    wl03_1_cross_wei=ifelse(wl03_1==1, 4,
                            ifelse(wl03_1==2, 3,
                                   ifelse(wl03_1==3, 2,
                                          ifelse(wl03_1==4, 1,
                                                 ifelse(wl03_1==5, 0,
                                                        ifelse(is.na(wl03_1), NA, NA)))))),
    wl11_0_cross_wei=ifelse(wl11==1, 4,
                            ifelse(wl11==2, 3,
                                   ifelse(wl11==3, 2,
                                          ifelse(wl11==4, 1,
                                                 ifelse(wl11==5, 0,
                                                        ifelse(is.na(wl11), NA, NA)))))),
    wl12_0_cross_wei=ifelse(wl12_0==1, 4,
                            ifelse(wl12_0==2, 3,
                                   ifelse(wl12_0==3, 2,
                                          ifelse(wl12_0==4, 1,
                                                 ifelse(wl12_0==5, 0,
                                                        ifelse(is.na(wl12_0), NA, NA)))))),
    wl13_cross_wei=ifelse(wl13==1, 4,
                          ifelse(wl13==2, 3,
                                 ifelse(wl13==3, 0,
                                        ifelse(is.na(wl13), NA, NA)))),
    
    zw06_1_cross_wei=ifelse(zw06_1==1, 4,
                            ifelse(zw06_1==2, 3,
                                   ifelse(zw06_1==3, 0,
                                          ifelse(is.na(zw06_1), NA, NA)))),
    
    zw10_1_cross_wei=ifelse(zw10_1==1, 4,
                            ifelse(zw10_1==2, 3,
                                   ifelse(zw10_1==3, 0,
                                          ifelse(is.na(zw10_1), NA, NA)))),
    
    samenredzaam_ss=mean(c(wl03_1_cross_wei,wl11_0_cross_wei,wl12_0_cross_wei,wl13_cross_wei, zw06_1_cross_wei, zw10_1_cross_wei),na.rm=TRUE) * (10/4),
    
    
    #_______________________________________________________________________
    #Schaalscore sociale relaties
    
    deficit_ss=ifelse(zw03==1, 10,
                      ifelse(zw03==2, 5,
                             ifelse(zw03==3, 0,
                                    ifelse(zw03==4, NA,
                                           ifelse(is.na(zw03), NA, NA))))),
    
    #eenzaamheid
    alone_ss=ifelse(zw04==1, 10,
                    ifelse(zw04==2, 8,
                           ifelse(zw04==3, 6,
                                  ifelse(zw04==4, 0,
                                         ifelse(zw04==5, NA,
                                                ifelse(is.na(zw04), NA, NA)))))),
    
    #uitsluiting
    zw01_5_wei=ifelse(zw01_5==1, 5,
                      ifelse(zw01_5==2, 4,
                             ifelse(zw01_5==3, 2,
                                    ifelse(zw01_5==4, 0,
                                           ifelse(is.na(zw01_5), NA, NA))))),
    
    socrel_ss=mean(c(deficit_ss,alone_ss,zw01_5_wei),na.rm=TRUE),
    
    
    #_______________________________________________________________________
    #Schaalscore sociaal vangnet 
    
    zw10_0_wei=ifelse(zw10_0==1, 10,
                      ifelse(zw10_0==2, 5,
                             ifelse(zw10_0==3, 0,
                                    ifelse(is.na(zw10_0), NA, NA)))),
    
    zw10_1_wei=ifelse(zw10_1==1, 10,
                      ifelse(zw10_1==2, 5,
                             ifelse(zw10_1==3, 0,
                                    ifelse(is.na(zw10_1), NA, NA)))),
    
    vangnet_ss=mean(c(zw10_0_wei,zw10_1_wei),na.rm=TRUE),
    
    
    #_______________________________________________________________________
    #Schaalscore sociale veiligheid
    
    wl04_wei=ifelse(wl04==1, 8,
                    ifelse(wl04==2, 6,
                           ifelse(wl04==3, 4,
                                  ifelse(wl04==4, 2,
                                         ifelse(wl04==5, 0,
                                                ifelse(is.na(wl04), NA, NA)))))),
    
    wl05_wei=ifelse(wl05==1, 0,
                    ifelse(wl05==2, 2,
                           ifelse(wl05==3, 4,
                                  ifelse(wl05==4, 6,
                                         ifelse(wl05==5, 8,
                                                ifelse(is.na(wl05), NA, NA)))))),
    
    wl06_wei=ifelse(wl06==1, 0,
                    ifelse(wl06==2, 2,
                           ifelse(wl06==3, 4,
                                  ifelse(wl06==4, 6,
                                         ifelse(is.na(wl06), NA, NA))))),
    
    safe_ss=mean(c(wl04_wei,wl05_wei,wl06_wei),na.rm=TRUE) * (5/4),
    
    
    #_______________________________________________________________________
    #Schaalscore Fysieke leefomgevingskwaliteit
    
    wl07_0_wei=ifelse(wl07_0==1, 4,
                      ifelse(wl07_0==2, 3,
                             ifelse(wl07_0==3, 2,
                                    ifelse(wl07_0==4, 1,
                                           ifelse(wl07_0==5, 0,
                                                  ifelse(is.na(wl07_0), NA, NA)))))),
    wl07_1_wei=ifelse(wl07_1==1, 4,
                      ifelse(wl07_1==2, 3,
                             ifelse(wl07_1==3, 2,
                                    ifelse(wl07_1==4, 1,
                                           ifelse(wl07_1==5, 0,
                                                  ifelse(is.na(wl07_1), NA, NA)))))),
    
    wl08_0_wei=ifelse(wl08_0==1, 4,
                      ifelse(wl08_0==2, 3,
                             ifelse(wl08_0==3, 2,
                                    ifelse(wl08_0==4, 1,
                                           ifelse(wl08_0==5, 0,
                                                  ifelse(is.na(wl08_0), NA, NA)))))),
    
    wl08_1_wei=ifelse(wl08_1==1, 4,
                      ifelse(wl08_1==2, 3,
                             ifelse(wl08_1==3, 2,
                                    ifelse(wl08_1==4, 1,
                                           ifelse(wl08_1==5, 0,
                                                  ifelse(is.na(wl08_1), NA, NA)))))),
    
    vz01_0_wei=ifelse(vz01_0==1, 4,
                      ifelse(vz01_0==2, 3,
                             ifelse(vz01_0==3, 2,
                                    ifelse(vz01_0==4, 1,
                                           ifelse(vz01_0==5, 0,
                                                  ifelse(is.na(vz01_0), NA, NA)))))),
    
    fk_ss=mean(c(wl07_0_wei, wl07_1_wei,wl08_0_wei,wl08_1_wei, vz01_0_wei),na.rm=TRUE) * (35/14),
    
    
    #_______________________________________________________________________
    #Schaalscore voorzieningenniveau.
    
    
    vz01_1_wei=ifelse(vz01_1==1, 4,
                      ifelse(vz01_1==2, 3,
                             ifelse(vz01_1==3, 2,
                                    ifelse(vz01_1==4, 1,
                                           ifelse(vz01_1==5, 0,
                                                  ifelse(is.na(vz01_1), NA, NA)))))),
    
    vz02_0_wei=ifelse(vz02_0==1, 4,
                      ifelse(vz02_0==2, 3,
                             ifelse(vz02_0==3, 2,
                                    ifelse(vz02_0==4, 1,
                                           ifelse(vz02_0==5, 0,
                                                  ifelse(is.na(vz02_0), NA, NA)))))),
    
    vz02_1_wei=ifelse(vz02_1==1, 4,
                      ifelse(vz02_1==2, 3,
                             ifelse(vz02_1==3, 2,
                                    ifelse(vz02_1==4, 1,
                                           ifelse(vz02_1==5, 0,
                                                  ifelse(is.na(vz02_1), NA, NA)))))),
    
    vz03_0_wei=ifelse(vz03_0==1, 4,
                      ifelse(vz03_0==2, 3,
                             ifelse(vz03_0==3, 2,
                                    ifelse(vz03_0==4, 1,
                                           ifelse(vz03_0==5, 0,
                                                  ifelse(is.na(vz03_0), NA, NA)))))),
    
    vz03_1_wei=ifelse(vz03_1==1, 4,
                      ifelse(vz03_1==2, 3,
                             ifelse(vz03_1==3, 2,
                                    ifelse(vz03_1==4, 1,
                                           ifelse(vz03_1==5, 0,
                                                  ifelse(is.na(vz03_1), NA, NA)))))),
    
    vz03_2_wei=ifelse(vz03_2==1, 4,
                      ifelse(vz03_2==2, 3,
                             ifelse(vz03_2==3, 2,
                                    ifelse(vz03_2==4, 1,
                                           ifelse(vz03_2==5, 0,
                                                  ifelse(is.na(vz03_2), NA, NA)))))),
    
    vz03_3_wei=ifelse(vz03_3==1, 4,
                      ifelse(vz03_3==2, 3,
                             ifelse(vz03_3==3, 2,
                                    ifelse(vz03_3==4, 1,
                                           ifelse(vz03_3==5, 0,
                                                  ifelse(is.na(vz03_3), NA, NA)))))),
    
    vz03_4_wei=ifelse(vz03_4==1, 4,
                      ifelse(vz03_4==2, 3,
                             ifelse(vz03_4==3, 2,
                                    ifelse(vz03_4==4, 1,
                                           ifelse(vz03_4==5, 0,
                                                  ifelse(is.na(vz03_4), NA, NA)))))),
    
    voorzieningen_ss=mean(c(vz01_0_wei, vz01_1_wei, vz02_0_wei,vz02_1_wei, vz03_0_wei, vz03_1_wei, vz03_2_wei, vz03_3_wei, vz03_4_wei),na.rm=TRUE) * (35/14),
    
    
    #_______________________________________________________________________
    #Schaalscore verbondenheid met de buurt
    
    wl02_0_wei=ifelse(wl02_0==1, 4,
                      ifelse(wl02_0==2, 3,
                             ifelse(wl02_0==3, 2,
                                    ifelse(wl02_0==4, 1,
                                           ifelse(wl02_0==5, 0,
                                                  ifelse(is.na(wl02_0), NA, NA)))))),
    
    wl02_1_wei=ifelse(wl02_1==1, 4,
                      ifelse(wl02_1==2, 3,
                             ifelse(wl02_1==3, 2,
                                    ifelse(wl02_1==4, 1,
                                           ifelse(wl02_1==5, 0,
                                                  ifelse(is.na(wl02_1), NA, NA)))))),
    
    wl03_0_wei=ifelse(wl03_0==1, 4,
                      ifelse(wl03_0==2, 3,
                             ifelse(wl03_0==3, 2,
                                    ifelse(wl03_0==4, 1,
                                           ifelse(wl03_0==5, 0,
                                                  ifelse(is.na(wl03_0), NA, NA)))))),
    
    wl03_1_wei=ifelse(wl03_1==1, 4,
                      ifelse(wl03_1==2, 3,
                             ifelse(wl03_1==3, 2,
                                    ifelse(wl03_1==4, 1,
                                           ifelse(wl03_1==5, 0,
                                                  ifelse(is.na(wl03_1), NA, NA)))))),
    
    verbonden_ss=mean(c(wl02_0_wei,wl02_1_wei,wl03_0_wei,wl03_1_wei),na.rm=TRUE) * (10/4),
    
    #_______________________________________________________________________
    #Schaalscore vertrouwen in de gemeente
    
    trust_ss=ifelse(bo01==1, 10,
                    ifelse(bo01==2, 8,
                           ifelse(bo01==3, 6,
                                  ifelse(bo01==4, 4,
                                         ifelse(bo01==5, 2,
                                                ifelse(is.na(bo01), NA, NA)))))),
    #_______________________________________________________________________
    #Schaalscore betrokken, oplossingsgerichte en betrouwbare gemeente (BOB).
    
    bo02_0_wei=ifelse(bo02_0==1, 4,
                      ifelse(bo02_0==2, 3,
                             ifelse(bo02_0==3, 2,
                                    ifelse(bo02_0==4, 1,
                                           ifelse(bo02_0==5, 0,
                                                  ifelse(is.na(bo02_0), NA, NA)))))),
    
    bo02_1_wei=ifelse(bo02_1==1, 4,
                      ifelse(bo02_1==2, 3,
                             ifelse(bo02_1==3, 2,
                                    ifelse(bo02_1==4, 1,
                                           ifelse(bo02_1==5, 0,
                                                  ifelse(is.na(bo02_1), NA, NA)))))),
    
    bo02_2_wei=ifelse(bo02_2==1, 4,
                      ifelse(bo02_2==2, 3,
                             ifelse(bo02_2==3, 2,
                                    ifelse(bo02_2==4, 1,
                                           ifelse(bo02_2==5, 0,
                                                  ifelse(is.na(bo02_2), NA, NA)))))),
    
    bo03_0_wei=ifelse(bo03_0==1, 4,
                      ifelse(bo03_0==2, 3,
                             ifelse(bo03_0==3, 2,
                                    ifelse(bo03_0==4, 1,
                                           ifelse(bo03_0==5, 0,
                                                  ifelse(is.na(bo03_0), NA, NA)))))),
    
    bo03_1_wei=ifelse(bo03_1==1, 4,
                      ifelse(bo03_1==2, 3,
                             ifelse(bo03_1==3, 2,
                                    ifelse(bo03_1==4, 1,
                                           ifelse(bo03_1==5, 0,
                                                  ifelse(is.na(bo03_1), NA, NA)))))),
    
    bo04_0_wei=ifelse(bo04_0==1, 4,
                      ifelse(bo04_0==2, 3,
                             ifelse(bo04_0==3, 2,
                                    ifelse(bo04_0==4, 1,
                                           ifelse(bo04_0==5, 0,
                                                  ifelse(is.na(bo04_0), NA, NA)))))),
    
    wl12_0_wei=ifelse(wl12_0==1, 4,
                      ifelse(wl12_0==2, 3,
                             ifelse(wl12_0==3, 2,
                                    ifelse(wl12_0==4, 1,
                                           ifelse(wl12_0==5, 0,
                                                  ifelse(is.na(wl12_0), NA, NA)))))),
    
    wl12_2_wei=ifelse(wl12_2==1, 4,
                      ifelse(wl12_2==2, 3,
                             ifelse(wl12_2==3, 2,
                                    ifelse(wl12_2==4, 1,
                                           ifelse(wl12_2==5, 0,
                                                  ifelse(is.na(wl12_2), NA, NA)))))),
    
    betrouwbaar_ss=mean(c(bo02_0_wei,bo02_1_wei,bo02_2_wei,bo03_0_wei,bo03_1_wei,bo04_0_wei, wl12_0_wei, wl12_2_wei),na.rm=TRUE) * (10/4),
    
    
    #_______________________________________________________________________
    #Schaalscore maatschappelijke inzet
    
    act_ver_intens=sum(zw05_0, zw05_1, zw05_2, zw05_3, zw05_5,na.rm=TRUE),
    
    act_ver_wei=ifelse(act_ver_intens==1, 2,
                       ifelse(act_ver_intens>1, 4,
                              ifelse(is.na(act_ver_intens), NA, 0))),
    
    act_mz_wei=ifelse(zw06_0==1, 4,
                      ifelse(zw06_0==2, 3,
                             ifelse(zw06_0==3, 0,
                                    ifelse(is.na(zw06_0), NA, NA)))),
    
    act_hlp_wei=ifelse(zw06_1==1, 4,
                       ifelse(zw06_1==2, 3,
                              ifelse(zw06_1==3, 0,
                                     ifelse(is.na(zw06_1), NA, NA)))),
    
    act_zw_wei=ifelse(zw06_2==1, 4,
                      ifelse(zw06_2==2, 3,
                             ifelse(zw06_2==3, 0,
                                    ifelse(is.na(zw06_2), NA, NA)))),
    
    act_vw_wei=ifelse(zw06_3==1, 4,
                      ifelse(zw06_3==2, 3,
                             ifelse(zw06_3==3, 0,
                                    ifelse(is.na(zw06_3), NA, NA)))),
    
    par_ss=mean(c(act_ver_wei,act_mz_wei,act_hlp_wei,act_zw_wei,act_vw_wei),na.rm=TRUE) * (20/8),
    
    
    #_______________________________________________________________________
    #Schaalscore Eigen kracht (Leven zonder beperkingen) 
    
    bep_gez_wei=ifelse(zw01_0==1, 4,
                       ifelse(zw01_0==2, 3,
                              ifelse(zw01_0==3, 2,
                                     ifelse(zw01_0==4, 0,
                                            ifelse(zw01_0==5, NA,
                                                   ifelse(is.na(zw01_0), NA, NA)))))),
    bep_fys_wei=ifelse(zw01_1==1, 4,
                       ifelse(zw01_1==2, 3,
                              ifelse(zw01_1==3, 2,
                                     ifelse(zw01_1==4, 0,
                                            ifelse(zw01_1==5, NA,
                                                   ifelse(is.na(zw01_1), NA, NA)))))),
    bep_men_wei=ifelse(zw01_2==1, 4,
                       ifelse(zw01_2==2, 3,
                              ifelse(zw01_2==3, 2,
                                     ifelse(zw01_2==4, 0,
                                            ifelse(zw01_2==5, NA,
                                                   ifelse(is.na(zw01_2), NA, NA)))))),
    bep_cul_wei=ifelse(zw01_3==1, 4,
                       ifelse(zw01_3==2, 3,
                              ifelse(zw01_3==3, 2,
                                     ifelse(zw01_3==4, 0,
                                            ifelse(zw01_3==5, NA,
                                                   ifelse(is.na(zw01_3), NA, NA)))))),
    bep_ink_wei=ifelse(zw01_4==1, 4,
                       ifelse(zw01_4==2, 3,
                              ifelse(zw01_4==3, 2,
                                     ifelse(zw01_4==4, 0,
                                            ifelse(zw01_4==5, NA,
                                                   ifelse(is.na(zw01_4), NA, NA)))))),
    bep_uit_wei=ifelse(zw01_5==1, 4,
                       ifelse(zw01_5==2, 3,
                              ifelse(zw01_5==3, 2,
                                     ifelse(zw01_5==4, 0,
                                            ifelse(zw01_5==5, NA,
                                                   ifelse(is.na(zw01_5), NA, NA)))))),
    
    bep_and_wei=ifelse(zw01_6==1, 4,
                       ifelse(zw01_6==2, 3,
                              ifelse(zw01_6==3, 2,
                                     ifelse(zw01_6==4, 0,
                                            ifelse(zw01_6==5, NA,
                                                   ifelse(is.na(zw01_6), NA, NA)))))),
    
    #dimensie beperkingen op het vlak van gezondheid en fysiek functioneren
    bep_dim1_ss=min(c(bep_gez_wei,bep_fys_wei,bep_and_wei),na.rm=TRUE),
    #dimensie soc-cult-econ beperkingen 
    bep_dim2_ss=min(c(bep_men_wei,bep_cul_wei,bep_ink_wei,bep_uit_wei),na.rm=TRUE),
    kunnen_ss=mean(c(bep_dim1_ss,bep_dim2_ss),na.rm=TRUE)*5/2,
    
    
    #_______________________________________________________________________
    #Schaalscore Sociale veerkracht
    
    zw19_0_wei=ifelse(zw19_0==1, 4,
                      ifelse(zw19_0==2, 3,
                             ifelse(zw19_0==3, 2,
                                    ifelse(zw19_0==4, 1,
                                           ifelse(zw19_0==5, 0,
                                                  ifelse(is.na(zw19_0), NA, NA)))))),
    zw19_1_wei=ifelse(zw19_1==1, 4,
                      ifelse(zw19_1==2, 3,
                             ifelse(zw19_1==3, 2,
                                    ifelse(zw19_1==4, 1,
                                           ifelse(zw19_1==5, 0,
                                                  ifelse(is.na(zw19_1), NA, NA)))))),
    zw19_2_wei=ifelse(zw19_2==1, 4,
                      ifelse(zw19_2==2, 3,
                             ifelse(zw19_2==3, 2,
                                    ifelse(zw19_2==4, 1,
                                           ifelse(zw19_2==5, 0,
                                                  ifelse(is.na(zw19_2), NA, NA)))))),
    zw19_3_wei=ifelse(zw19_3==1, 4,
                      ifelse(zw19_3==2, 3,
                             ifelse(zw19_3==3, 2,
                                    ifelse(zw19_3==4, 1,
                                           ifelse(zw19_3==5, 0,
                                                  ifelse(is.na(zw19_3), NA, NA)))))),
    zw19_4_wei=ifelse(zw19_4==1, 4,
                      ifelse(zw19_4==2, 3,
                             ifelse(zw19_4==3, 2,
                                    ifelse(zw19_4==4, 1,
                                           ifelse(zw19_4==5, 0,
                                                  ifelse(is.na(zw19_4), NA, NA)))))),
    
    sv_ss=mean(c(zw19_0_wei,zw19_1_wei,zw19_2_wei,zw19_3_wei,zw19_4_wei), na.rm=TRUE) * (35/14),        
    
    
    #_______________________________________________________________________
    #Schaalscore Hostmanship
    
    mm01_0_wei=ifelse(mm01_0==1, 4,
                      ifelse(mm01_0==2, 3,
                             ifelse(mm01_0==3, 2,
                                    ifelse(mm01_0==4, 1,
                                           ifelse(mm01_0==5, 0,
                                                  ifelse(is.na(mm01_0), NA, NA)))))),
    
    mm01_1_wei=ifelse(mm01_1==1, 4,
                      ifelse(mm01_1==2, 3,
                             ifelse(mm01_1==3, 2,
                                    ifelse(mm01_1==4, 1,
                                           ifelse(mm01_1==5, 0,
                                                  ifelse(is.na(mm01_1), NA, NA)))))),
    
    mm01_2_wei=ifelse(mm01_2==1, 4,
                      ifelse(mm01_2==2, 3,
                             ifelse(mm01_2==3, 2,
                                    ifelse(mm01_2==4, 1,
                                           ifelse(mm01_2==5, 0,
                                                  ifelse(is.na(mm01_2), NA, NA)))))),
    
    
    mm01_3_wei=ifelse(mm01_3==1, 4,
                      ifelse(mm01_3==2, 3,
                             ifelse(mm01_3==3, 2,
                                    ifelse(mm01_3==4, 1,
                                           ifelse(mm01_3==5, 0,
                                                  ifelse(is.na(mm01_3), NA, NA)))))),
    
    
    mm01_4_wei=ifelse(mm01_4==1, 4,
                      ifelse(mm01_4==2, 3,
                             ifelse(mm01_4==3, 2,
                                    ifelse(mm01_4==4, 1,
                                           ifelse(mm01_4==5, 0,
                                                  ifelse(is.na(mm01_4), NA, NA)))))),
    
    mm02_0_wei=ifelse(mm02_0==1, 4,
                      ifelse(mm02_0==2, 3,
                             ifelse(mm02_0==3, 2,
                                    ifelse(mm02_0==4, 1,
                                           ifelse(mm02_0==5, 0,
                                                  ifelse(is.na(mm02_0), NA, NA)))))),
    
    
    hostman_ss=mean(c(mm01_0_wei, mm01_1_wei, mm01_2_wei, mm01_3_wei, mm01_4_wei, mm02_0_wei),na.rm=TRUE) * (10/4),
    
    
    #_______________________________________________________________________
    #schaalscore vitaliteit en welzijn
    
    #person
    welzijn_ss=mean(c(ses_ss,socrel_ss,zw02,zw00,kunnen_ss,bereid_ss,par_ss,vangnet_ss,sv_ss),na.rm=TRUE),
    #environment/community
    vitaliteit_ss=mean(c(wl01,verbonden_ss,samenredzaam_ss,safe_ss,fk_ss,voorzieningen_ss,betrouwbaar_ss),na.rm=TRUE),
    #combined
    vitaliteitwelzijn_ss=mean(c(welzijn_ss,vitaliteit_ss), na.rm=TRUE),  
    
    #_______________________________________________________________________
    #maatschappelijke participatie (breedte-maat)
    part_score=sum(c(act_ver,act_vw,act_neigh,act_opl,act_arb,act_soc,act_ih,act_hlp), na.rm=TRUE),
    #individueel welzijn (breedte-maat)
    qol_score=sum(c(health,bep_lich_neg,bep_soc_neg,alone_neg,sv_pos,pleasant_neigh,safeguard), na.rm=TRUE)
    
    #_______________________________________________________________________
  ) %>%
  collect()  %>%
  mutate_at(.,vars(-group_cols()),~ifelse(is.nan(.) | is.infinite(.), NA, .)) %>%
  #mutate_at(.,vars(-group_cols()),~replace(., .== 0, NA)) %>% 
  ungroup() %>% 
  select(-GEMEENTE)