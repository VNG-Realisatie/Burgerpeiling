
#-----------------------------------------------------------------------------------------------

#Recode

#-----------------------------------------------------------------------------------------------

#multiple response sets (missings -> 0)
mr<-c("dv03_0","dv03_1","dv03_2","dv03_3","dv03_4","dv03_5","dv03_6",
      "zw05_0","zw05_1","zw05_2","zw05_3","zw05_4","zw05_5",
      "zw08_0","zw08_1","zw08_2","zw08_3","zw08_4","zw08_5","zw08_6",#"zw08_7",
      "zw13_0","zw13_1","zw13_2","zw13_3","zw13_4","zw13_5","zw13_6","zw13_7","zw13_8"
)
df<-df %>% mutate(across(any_of(mr), ~replace_na(.,0)))

#Buurtparticipatie, vrijwilligerswerk.
#1:nu en wellicht toekomst.
#2:nu maar niet in de toekomst.
#3:nu niet maar wellicht wel in de toekomst.
#4:niet nu en niet in de toekomst

df<- df %>% 
  mutate(wl14_dum=ifelse(
    wl14== 4,NA,wl14),
    zw07_dum=ifelse(
      zw07== 4,NA,zw07),
    zw05_rc= ifelse(
      zw05_4== 0,1,NA),
    #buurtparticipatie
    part_brt=ifelse(wl13<3 & wl14_dum<3, 1,
                    ifelse(wl13<3 & wl14_dum==3, 2,
                           ifelse(wl13==3 & wl14_dum<3, 3,
                                  ifelse(wl13==3 & wl14_dum==3, 4, 0)))),
    #vrijwilligerswerk
    part_vw=ifelse(zw06_3<3 & zw07_dum<3, 1,
                   ifelse(zw06_3<3 & zw07_dum==3, 2,
                          ifelse(zw06_3==3 & zw07_dum<3, 3,
                                 ifelse(zw06_3==3 & zw07_dum==3, 4, 0)))),
    #leeftijd
    lft_cy=ifelse(ch02==2, 1,
                  ifelse(ch02==3, 2,
                         ifelse(ch02==4, 2,
                                ifelse(ch02==5, 3,
                                       ifelse(ch02==6, 4, NA)))))
  ) 

