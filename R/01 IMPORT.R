#-----------------------------------------------------------------------------------------------

# Burgerpeiling Waarstaatjegemeente.nl

#-----------------------------------------------------------------------------------------------

#this procedure is to check the results of the Burgerpeiling as presented on
#Waarstaatjegemeente.nl

#see'Beschrijving' directory for specification of the variables.

#last update 2022-10-21 (alpha version)

#-----------------------------------------------------------------------------------------------

# Setup environment

#-----------------------------------------------------------------------------------------------

#Run in project environment (to avoid package conflicts)
proj_env<-FALSE #default (F)

#packages
source('SRC/packages.R')

source(here::here('SRC/globals.R'))

#-----------------------------------------------------------------------------------------------

# Import 

#-----------------------------------------------------------------------------------------------

#read Spss sav-file(s)
file_type<-'sav'

qry<-paste0("*",file_type)
files<- fs::dir_ls(glob=qry, path=data.dir)


df<- map_df(set_names(files), function(file) {
  file %>% 
    map_df(
      ~ haven::read_sav(file) %>% as_tibble()
    ) %>% 
    #Weight 
    filter(weging<5) 
})

#-----------------------------------------------------------------------------------------------

#pipeline timer
start_time<-Sys.time()

#-----------------------------------------------------------------------------------------------

# MISSINGS

#-----------------------------------------------------------------------------------------------

#missing values analysis
colSums(is.na(df))

#identify numeric columns
num_cols<- unlist(lapply(df, is.numeric))         
num_cols

#select numeric columns 
df<-df[,num_cols]

ncol(df)

#create missing values for assigned values (77,88,99,-1), including 11 (weet niet/geen mening)
df[,9:ncol(df)][df[,9:ncol(df)]>10]<- NA
df[,9:ncol(df)][df[,9:ncol(df)]== -1]<- NA

#missing cat located at 6th position
#recode 6th cat (weet niet/geen mening) to NA for variables with exact 6 cats 
colMax<- function(df) sapply(df, max, na.rm=TRUE)
cols<-ifelse(colMax(df)==6,TRUE,FALSE)

#except for leeftijd and opleiding
cols[102]<-FALSE
cols[103]<-FALSE

df[,cols][df[,cols]== 6]<- NA

#missing cat located at 7th position
ml7<-c('ch06')
df[,ml7][df[,ml7]==7]<- NA

#missing cat located at 5th position
ml5<-c('wl06','zw01_0','zw01_1','zw01_2','zw01_3','zw01_4','zw01_5','zw01_6', 'zw09')
df[,ml5][df[,ml5]==5]<- NA

#missing cat located at 4th position
ml4<-c('wl14','zw03','zw07','zw10_0','zw10_1','zw10_2')
df[,ml4][df[,ml4]==4]<- NA

#reorder variables for readability
df<- df %>%
  select(sort(names(.))) %>% 
  relocate(any_of(c('gemnr', 'jr', 'weging', 'kw', 'veldwerkmodus', 'wijk', 'org')), .before=bo01) %>% 
  rename(PERIOD=jr, GEOITEM=gemnr)


#-----------------------------------------------------------------------------------------------

# Factors levels

#-----------------------------------------------------------------------------------------------

#read municipality names
file_type<-'xlsx'
qry<-paste0("*",file_type)
files<- fs::dir_ls(glob=qry, path="CBS")

gemeenten_meta<- map_df(set_names(files), function(file) {
  file %>% 
    map_df(
      ~ readxl::read_xlsx(path=file, col_names=TRUE, sheet=1, trim_ws=TRUE, guess_max=20))
})

gemeenten_meta$Gemeentecode<-as.numeric(gemeenten_meta$Gemeentecode)

gem_levels<-gemeenten_meta$Gemeentecode
gem_labels<-gemeenten_meta$Gemeentenaam

df$GEMEENTE<- factor(df$GEOITEM, levels=gem_levels, labels=gem_labels)
#df$GEOITEM<- factor(df$GEOITEM)
#df$jr<- factor(df$jr)

df<-df %>%
  relocate(any_of(c('GEMEENTE')), .before=GEOITEM)


#-----------------------------------------------------------------------------------------------

# Recode

#-----------------------------------------------------------------------------------------------

#multiple response sets (missings -> 0)
mr<-c("dv03_0","dv03_1","dv03_2","dv03_3","dv03_4","dv03_5","dv03_6",
      "zw05_0","zw05_1","zw05_2","zw05_3","zw05_4","zw05_5",
      "zw08_0","zw08_1","zw08_2","zw08_3","zw08_4","zw08_5","zw08_6",
      "zw13_0","zw13_1","zw13_2","zw13_3","zw13_4","zw13_5","zw13_6","zw13_7","zw13_8"
      )
df<-df %>% mutate(across(all_of(mr), ~replace_na(.,0)))

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
                                              ifelse(ch02==6, 4,0)))))
         ) 

#-----------------------------------------------------------------------------------------------

# Respondent ID

#-----------------------------------------------------------------------------------------------

#set sequence number
df$seq<-row.names(df)

#set unique identifier
df$id<-paste0("BP",df$GEOITEM,"Y",df$PERIOD,"S",df$seq)

#-----------------------------------------------------------------------------------------------

# Aggregate

#-----------------------------------------------------------------------------------------------

#apply weight to df
df_weight<- df %>% 
  srvyr::as_survey_design(ids=1, # 1 for no cluster ids 
                          weights=weging, # weight added
                          strata=NULL) # sampling was simple (no strata)

#Report variability as one or more of: standard error ("se", default), 
#confidence interval ("ci"), variance ("var") or coefficient of variation ("cv").
vt<-NULL

#-----------------------------------------------------------------------------------------------
#MEAN (gemeente, jaar)

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
    mutate_at(.,vars(-group_cols()),~replace(., .== 0, NA))  
  
#df_aggr_mn2<- df_weight %>%
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


#-----------------------------------------------------------------------------------------------
#PIN (gemeente, jaar)

df_aggr_pin<- df_weight %>%
  group_by(GEOITEM,PERIOD) %>%
  srvyr::summarise(
    wl01_pin=(survey_total((wl01>=1 & wl01<=5), na.rm=T) / n()*100),
    wl01_pin_1=(survey_total((wl01>=1 & wl01<=5), na.rm=T) / n()*100),
    wl01_pin_2=(survey_total((wl01==6), na.rm=T) / n()*100),
    wl01_pin_3=(survey_total((wl01==7), na.rm=T) / n()*100),
    wl01_pin_4=(survey_total((wl01==8), na.rm=T) / n()*100),
    wl01_pin_5=(survey_total((wl01 %in% c(9,10)), na.rm=T) / n()*100),
    wl16_pin=(survey_total((wl16>=1 & wl16<=5), na.rm=T) / n()*100),
    wl16_pin_1=(survey_total((wl16>=1 & wl16<=5), na.rm=T) / n()*100),
    wl16_pin_2=(survey_total((wl16==6), na.rm=T) / n()*100),
    wl16_pin_3=(survey_total((wl16==7), na.rm=T) / n()*100),
    wl16_pin_4=(survey_total((wl16==8), na.rm=T) / n()*100),
    wl16_pin_5=(survey_total((wl16 %in% c(9,10)), na.rm=T) / n()*100),
    bo06_pin=(survey_total((bo06>=1 & bo06<=5), na.rm=T) / n()*100),
    bo06_pin_1=(survey_total((bo06>=1 & bo06<=5), na.rm=T) / n()*100),
    bo06_pin_2=(survey_total((bo06==6), na.rm=T) / n()*100),
    bo06_pin_3=(survey_total((bo06==7), na.rm=T) / n()*100),
    bo06_pin_4=(survey_total((bo06==8), na.rm=T) / n()*100),
    bo06_pin_5=(survey_total((bo06 %in% c(9,10)), na.rm=T) / n()*100),
    dv01_pin=(survey_total((dv01>=1 & dv01<=5), na.rm=T) / n()*100),
    dv01_pin_1=(survey_total((dv01>=1 & dv01<=5), na.rm=T) / n()*100),
    dv01_pin_2=(survey_total((dv01==6), na.rm=T) / n()*100),
    dv01_pin_3=(survey_total((dv01==7), na.rm=T) / n()*100),
    dv01_pin_4=(survey_total((dv01==8), na.rm=T) / n()*100),
    dv01_pin_5=(survey_total((dv01 %in% c(9,10)), na.rm=T) / n()*100),
    sc02_pin=(survey_total((sc02>=1 & sc02<=5), na.rm=T) / n()*100),
    sc02_pin_1=(survey_total((sc02>=1 & sc02<=5), na.rm=T) / n()*100),
    sc02_pin_2=(survey_total((sc02==6), na.rm=T) / n()*100),
    sc02_pin_3=(survey_total((sc02==7), na.rm=T) / n()*100),
    sc02_pin_4=(survey_total((sc02==8), na.rm=T) / n()*100),
    sc02_pin_5=(survey_total((sc02 %in% c(9,10)), na.rm=T) / n()*100),
    dv06_pin=(survey_total((dv06>=1 & dv06<=5), na.rm=T) / n()*100),
    dv06_pin_1=(survey_total((dv06>=1 & dv06<=5), na.rm=T) / n()*100),
    dv06_pin_2=(survey_total((dv06==6), na.rm=T) / n()*100),
    dv06_pin_3=(survey_total((dv06==7), na.rm=T) / n()*100),
    dv06_pin_4=(survey_total((dv06==8), na.rm=T) / n()*100),
    dv06_pin_5=(survey_total((dv06 %in% c(9,10)), na.rm=T) / n()*100),
   dv10_pin_1=(survey_total((dv10>=1 & dv10<=5), na.rm=T) / n()*100),
   dv10_pin_2=(survey_total((dv10==6), na.rm=T) / n()*100),
   dv10_pin_3=(survey_total((dv10==7), na.rm=T) / n()*100),
   dv10_pin_4=(survey_total((dv10==8), na.rm=T) / n()*100),
   dv10_pin_5=(survey_total((dv10 %in% c(9,10)), na.rm=T) / n()*100),
   zw12_pin_1=(survey_total((zw12>=1 & zw12<=5), na.rm=T) / n()*100),
   zw12_pin_2=(survey_total((zw12==6), na.rm=T) / n()*100),
   zw12_pin_3=(survey_total((zw12==7), na.rm=T) / n()*100),
   zw12_pin_4=(survey_total((zw12==8), na.rm=T) / n()*100),
   zw12_pin_5=(survey_total((zw12 %in% c(9,10)), na.rm=T) / n()*100),
   wl09_pin1=(survey_total((wl09==1), na.rm=T) / n()*100),
   wl09_pin2=(survey_total((wl09==2), na.rm=T) / n()*100),
   wl09_pin3=(survey_total((wl09==3), na.rm=T) / n()*100),
   wl09_pin4=(survey_total((wl09==4), na.rm=T) / n()*100),
   wl09_pin5=(survey_total((wl09==5), na.rm=T) / n()*100),
   wl04_pin1=(survey_total((wl04==1), na.rm=T) / n()*100),
   wl04_pin2=(survey_total((wl04==2), na.rm=T) / n()*100),
   wl04_pin3=(survey_total((wl04==3), na.rm=T) / n()*100),
   wl04_pin4=(survey_total((wl04==4), na.rm=T) / n()*100),
   wl04_pin5=(survey_total((wl04==5), na.rm=T) / n()*100),
   wl05_pin1=(survey_total((wl05==1), na.rm=T) / n()*100),
   wl05_pin2=(survey_total((wl05==2), na.rm=T) / n()*100),
   wl05_pin3=(survey_total((wl05==3), na.rm=T) / n()*100),
   wl05_pin4=(survey_total((wl05==4), na.rm=T) / n()*100),
   wl05_pin5=(survey_total((wl05==5), na.rm=T) / n()*100),
   zw05_pin0=(survey_total((zw05_0==1), na.rm=T) / n()*100),
   zw05_pin1=(survey_total((zw05_1==1), na.rm=T) / n()*100),
   zw05_pin2=(survey_total((zw05_2==1), na.rm=T) / n()*100),
   zw05_pin3=(survey_total((zw05_3==1), na.rm=T) / n()*100),
   zw05_pin4=(survey_total((zw05_4==1), na.rm=T) / n()*100),
   zw05_pin5=(survey_total((zw05_5==1), na.rm=T) / n()*100),
   dv03_pin0=(survey_total((dv03_0==1), na.rm=T) / n()*100),
   dv03_pin1=(survey_total((dv03_1==1), na.rm=T) / n()*100),
   dv03_pin2=(survey_total((dv03_2==1), na.rm=T) / n()*100),
   dv03_pin3=(survey_total((dv03_3==1), na.rm=T) / n()*100),
   dv03_pin4=(survey_total((dv03_4==1), na.rm=T) / n()*100),
   dv03_pin5=(survey_total((dv03_5==1), na.rm=T) / n()*100),
   dv03_pin6=(survey_total((dv03_6==1), na.rm=T) / n()*100),
   zw08_0_pin=(survey_total((zw08_0==1), na.rm=T) / n()*100),
   zw08_1_pin=(survey_total((zw08_1==1), na.rm=T) / n()*100),
   zw08_2_pin=(survey_total((zw08_2==1), na.rm=T) / n()*100),
   zw08_3_pin=(survey_total((zw08_3==1), na.rm=T) / n()*100),
   zw08_4_pin=(survey_total((zw08_4==1), na.rm=T) / n()*100),
   zw08_5_pin=(survey_total((zw08_5==1), na.rm=T) / n()*100),
   zw08_6_pin=(survey_total((zw08_6==1), na.rm=T) / n()*100),
   zw13_0_pin=(survey_total((zw13_0==1), na.rm=T) / n()*100),
   zw13_1_pin=(survey_total((zw13_1==1), na.rm=T) / n()*100),
   zw13_2_pin=(survey_total((zw13_2==1), na.rm=T) / n()*100),
   zw13_3_pin=(survey_total((zw13_3==1), na.rm=T) / n()*100),
   zw13_4_pin=(survey_total((zw13_4==1), na.rm=T) / n()*100),
   zw13_5_pin=(survey_total((zw13_5==1), na.rm=T) / n()*100),
   zw13_6_pin=(survey_total((zw13_6==1), na.rm=T) / n()*100),
   zw13_7_pin=(survey_total((zw13_7==1), na.rm=T) / n()*100),
   zw13_8_pin=(survey_total((zw13_8==1), na.rm=T) / n()*100),
   zw13_9_pin=(survey_total((zw13_9==1), na.rm=T) / n()*100),
   zw13_10_pin=(survey_total((zw13_10==1), na.rm=T) / n()*100),
   zw01_0_pin1=(survey_total((zw01_0==1), na.rm=T) / n()*100),
   zw01_0_pin2=(survey_total((zw01_0==2), na.rm=T) / n()*100),
   zw01_0_pin3=(survey_total((zw01_0==3), na.rm=T) / n()*100),
   zw01_0_pin4=(survey_total((zw01_0==4), na.rm=T) / n()*100),
   zw01_1_pin1=(survey_total((zw01_1==1), na.rm=T) / n()*100),
   zw01_1_pin2=(survey_total((zw01_1==2), na.rm=T) / n()*100),
   zw01_1_pin3=(survey_total((zw01_1==3), na.rm=T) / n()*100),
   zw01_1_pin4=(survey_total((zw01_1==4), na.rm=T) / n()*100),
   zw01_2_pin1=(survey_total((zw01_2==1), na.rm=T) / n()*100),
   zw01_2_pin2=(survey_total((zw01_2==2), na.rm=T) / n()*100),
   zw01_2_pin3=(survey_total((zw01_2==3), na.rm=T) / n()*100),
   zw01_2_pin4=(survey_total((zw01_2==4), na.rm=T) / n()*100),
   zw01_3_pin1=(survey_total((zw01_3==1), na.rm=T) / n()*100),
   zw01_3_pin2=(survey_total((zw01_3==2), na.rm=T) / n()*100),
   zw01_3_pin3=(survey_total((zw01_3==3), na.rm=T) / n()*100),
   zw01_3_pin4=(survey_total((zw01_3==4), na.rm=T) / n()*100),
   zw01_4_pin1=(survey_total((zw01_4==1), na.rm=T) / n()*100),
   zw01_4_pin2=(survey_total((zw01_4==2), na.rm=T) / n()*100),
   zw01_4_pin3=(survey_total((zw01_4==3), na.rm=T) / n()*100),
   zw01_4_pin4=(survey_total((zw01_4==4), na.rm=T) / n()*100),
   zw01_5_pin1=(survey_total((zw01_5==1), na.rm=T) / n()*100),
   zw01_5_pin2=(survey_total((zw01_5==2), na.rm=T) / n()*100),
   zw01_5_pin3=(survey_total((zw01_5==3), na.rm=T) / n()*100),
   zw01_5_pin4=(survey_total((zw01_5==4), na.rm=T) / n()*100),
   zw01_6_pin1=(survey_total((zw01_6==1), na.rm=T) / n()*100),
   zw01_6_pin2=(survey_total((zw01_6==2), na.rm=T) / n()*100),
   zw01_6_pin3=(survey_total((zw01_6==3), na.rm=T) / n()*100),
   zw01_6_pin4=(survey_total((zw01_6==4), na.rm=T) / n()*100),
   zw10_0_pin1=(survey_total((zw10_0==1), na.rm=T) / n()*100),
   zw10_0_pin2=(survey_total((zw10_0==2), na.rm=T) / n()*100),
   zw10_0_pin3=(survey_total((zw10_0==3), na.rm=T) / n()*100),
   zw10_1_pin1=(survey_total((zw10_1==1), na.rm=T) / n()*100),
   zw10_1_pin2=(survey_total((zw10_1==2), na.rm=T) / n()*100),
   zw10_1_pin3=(survey_total((zw10_1==3), na.rm=T) / n()*100),
   zw10_2_pin1=(survey_total((zw10_2==1), na.rm=T) / n()*100),
   zw10_2_pin2=(survey_total((zw10_2==2), na.rm=T) / n()*100),
   zw10_2_pin3=(survey_total((zw10_2==3), na.rm=T) / n()*100),
   wl14_1=(survey_total((wl14==1), na.rm=T) / n()*100),
   wl14_2=(survey_total((wl14==2), na.rm=T) / n()*100),
   wl14_3=(survey_total((wl14==3), na.rm=T) / n()*100),
   #wl14_4=(survey_total((wl14==4), na.rm=T) / n()*100), 
   #dv04_pin1=(survey_total((dv04==1), na.rm=T) / n()*100),
   #dv04_pin2=(survey_total((dv04==2), na.rm=T) / n()*100),
   #dv04_pin3=(survey_total((dv04==3), na.rm=T) / n()*100),
   #dv04_pin4=(survey_total((dv04==4), na.rm=T) / n()*100),
   #dv04_pin5=(survey_total((dv04==5), na.rm=T) / n()*100),
   #dv04_pin6=(survey_total((dv04==6), na.rm=T) / n()*100),
   wl06_pin1=(survey_total((wl06==1), na.rm=T) / n()*100),
   wl06_pin2=(survey_total((wl06==2), na.rm=T) / n()*100),
   wl06_pin3=(survey_total((wl06==3), na.rm=T) / n()*100),
   wl06_pin4=(survey_total((wl06==4), na.rm=T) / n()*100),
   zw03_pin1=(survey_total((zw03==1), na.rm=T) / n()*100),
   zw03_pin2=(survey_total((zw03==2), na.rm=T) / n()*100),
   zw03_pin3=(survey_total((zw03==3), na.rm=T) / n()*100),
   zw04_pin1=(survey_total((zw04==1), na.rm=T) / n()*100),
   zw04_pin2=(survey_total((zw04==2), na.rm=T) / n()*100),
   zw04_pin3=(survey_total((zw04==3), na.rm=T) / n()*100), 
   zw04_pin4=(survey_total((zw04==4), na.rm=T) / n()*100),
   zw20_pin1=(survey_total((zw20==1), na.rm=T) / n()*100),
   zw20_pin2=(survey_total((zw20==2), na.rm=T) / n()*100),
   zw20_pin3=(survey_total((zw20==3), na.rm=T) / n()*100), 
   zw20_pin4=(survey_total((zw20==4), na.rm=T) / n()*100),
   zw20_pin5=(survey_total((zw20==5), na.rm=T) / n()*100),
   part_brt_pin1=(survey_total((part_brt==1), na.rm=T) / n()*100),
   part_brt_pin2=(survey_total((part_brt==2), na.rm=T) / n()*100),
   part_brt_pin3=(survey_total((part_brt==3), na.rm=T) / n()*100), 
   part_brt_pin4=(survey_total((part_brt==4), na.rm=T) / n()*100),
   part_brt_pin5=(survey_total((part_brt==5), na.rm=T) / n()*100),
   part_vw_pin1=(survey_total((part_vw==1), na.rm=T) / n()*100),
   part_vw_pin2=(survey_total((part_vw==2), na.rm=T) / n()*100),
   part_vw_pin3=(survey_total((part_vw==3), na.rm=T) / n()*100), 
   part_vw_pin4=(survey_total((part_vw==4), na.rm=T) / n()*100),
   part_vw_pin5=(survey_total((part_vw==5), na.rm=T) / n()*100),
   zw07_pin1=(survey_total((zw07==1), na.rm=T) / n()*100),
   zw07_pin2=(survey_total((zw07==2), na.rm=T) / n()*100),
   zw07_pin3=(survey_total((zw07==3), na.rm=T) / n()*100), 
   zw07_pin4=(survey_total((zw07==4), na.rm=T) / n()*100),
   zw09_pin1=(survey_total((zw09==1), na.rm=T) / n()*100),
   zw09_pin2=(survey_total((zw09==2), na.rm=T) / n()*100),
   zw09_pin3=(survey_total((zw09==3), na.rm=T) / n()*100), 
   zw09_pin4=(survey_total((zw09==4), na.rm=T) / n()*100),
   mz_lev1=(survey_total((zw06_0==1), na.rm=T) / n()*100),
   mz_lev2=(survey_total((zw06_0==2), na.rm=T) / n()*100),
   mz_lev3=(survey_total((zw06_0==3), na.rm=T) / n()*100),
   vw_lev1=(survey_total((zw06_3==1), na.rm=T) / n()*100),
   vw_lev2=(survey_total((zw06_3==2), na.rm=T) / n()*100),
   vw_lev3=(survey_total((zw06_3==3), na.rm=T) / n()*100),
   zw06_0=(survey_total((zw06_0<3), na.rm=T) / n()*100),
   zw06_1=(survey_total((zw06_1<3), na.rm=T) / n()*100),  
   zw06_2=(survey_total((zw06_2<3), na.rm=T) / n()*100), 
   zw06_3=(survey_total((zw06_3<3), na.rm=T) / n()*100), 
   zw19_0_pin=(survey_total((zw19_0<3), na.rm=T) / n()*100),
   zw19_1_pin=(survey_total((zw19_1<3), na.rm=T) / n()*100),
   zw19_2_pin=(survey_total((zw19_2<3), na.rm=T) / n()*100),
   zw19_3_pin=(survey_total((zw19_3<3), na.rm=T) / n()*100),
   zw19_4_pin=(survey_total((zw19_4<3), na.rm=T) / n()*100),
   vz03_0=(survey_total((vz03_0<3), na.rm=T) / n()*100),
   vz03_1=(survey_total((vz03_1<3), na.rm=T) / n()*100),
   vz03_2=(survey_total((vz03_2<3), na.rm=T) / n()*100),
   vz03_3=(survey_total((vz03_3<3), na.rm=T) / n()*100),
   vz03_4=(survey_total((vz03_4<3), na.rm=T) / n()*100),
   wl02_0=(survey_total((wl02_0<3), na.rm=T) / n()*100),
   wl02_1=(survey_total((wl02_1<3), na.rm=T) / n()*100),
   wl03_0=(survey_total((wl03_0<3), na.rm=T) / n()*100),
   wl03_1=(survey_total((wl03_1<3), na.rm=T) / n()*100),
   wl03_2=(survey_total((wl03_2<3), na.rm=T) / n()*100),
   wl07_0=(survey_total((wl07_0<3), na.rm=T) / n()*100),
   wl07_1=(survey_total((wl07_1<3), na.rm=T) / n()*100),
   wl08_0=(survey_total((wl08_0<3), na.rm=T) / n()*100),
   wl08_1=(survey_total((wl08_1<3), na.rm=T) / n()*100),
   wl12_0=(survey_total((wl12_0<3), na.rm=T) / n()*100),
   wl12_1=(survey_total((wl12_1<3), na.rm=T) / n()*100),
   wl12_2=(survey_total((wl12_2<3), na.rm=T) / n()*100),
   vz01_0=(survey_total((vz01_0<3), na.rm=T) / n()*100),
   vz01_1=(survey_total((vz01_1<3), na.rm=T) / n()*100),
   vz02_0=(survey_total((vz02_0<3), na.rm=T) / n()*100),
   vz02_1=(survey_total((vz02_1<3), na.rm=T) / n()*100),
   bo02_0=(survey_total((bo02_0<3), na.rm=T) / n()*100),
   bo02_1=(survey_total((bo02_1<3), na.rm=T) / n()*100),
   bo02_2=(survey_total((bo02_2<3), na.rm=T) / n()*100),
   bo03_0=(survey_total((bo03_0<3), na.rm=T) / n()*100),
   bo03_1=(survey_total((bo03_1<3), na.rm=T) / n()*100),
   bo04_0=(survey_total((bo04_0<3), na.rm=T) / n()*100),
   dv07_1=(survey_total((dv07_1<3), na.rm=T) / n()*100),
   dv07_2=(survey_total((dv07_2<3), na.rm=T) / n()*100), 
   dv07_3=(survey_total((dv07_3<3), na.rm=T) / n()*100),
   dv05_0=(survey_total((dv05_0<3), na.rm=T) / n()*100),   
   dv08_0=(survey_total((dv08_0<3), na.rm=T) / n()*100),
   dv09_0=(survey_total((dv09_0<3), na.rm=T) / n()*100),
   wl04=(survey_total((wl04<3), na.rm=T) / n()*100),
   wl05=(survey_total((wl05<3), na.rm=T) / n()*100),
   wl06=(survey_total((wl06<3), na.rm=T) / n()*100),
   wl09_rv=(survey_total((wl09==4 | wl09==5), na.rm=T) / n()*100),
   wl09=(survey_total((wl09<3), na.rm=T) / n()*100),
   wl11_0=(survey_total((wl11<3), na.rm=T) / n()*100),
   wl14=(survey_total((wl14<3), na.rm=T) / n()*100),
   bo01_rv=(survey_total((bo01==4 | bo01==5), na.rm=T) / n()*100),
   bo01=(survey_total((bo01<3), na.rm=T) / n()*100),
   zw00_pin=(survey_total((zw00 >6), na.rm=T) / n()*100),
   zw07=(survey_total((zw07<3), na.rm=T) / n()*100),
   #zw18_0_pin=(survey_total((zw18<3), na.rm=T) / n()*100),
   mm01_0=(survey_total((mm01_0<3), na.rm=T) / n()*100),
   mm01_1=(survey_total((mm01_1<3), na.rm=T) / n()*100),
   mm01_2=(survey_total((mm01_2<3), na.rm=T) / n()*100),
   mm01_3=(survey_total((mm01_3<3), na.rm=T) / n()*100),
   mm01_4=(survey_total((mm01_4<3), na.rm=T) / n()*100),
   mm02_0=(survey_total((mm02_0<3), na.rm=T) / n()*100),
   wl13=(survey_total((wl13==1), na.rm=T) / n()*100),
   dv02=(survey_total((dv02==1), na.rm=T) / n()*100),
   zw05_rc=(survey_total((zw05_4==0), na.rm=T) / n()*100),
   dv06_pin=(survey_total((dv06>=1 & dv06<=5), na.rm=T) / n()*100),
   dv10_pin=(survey_total((dv10>=1 & dv10<=5), na.rm=T) / n()*100),
   zw02_pin=(survey_total((zw02>=1 & zw02<=5), na.rm=T) / n()*100),
   zw12_pin=(survey_total((zw12>=1 & zw12<=5), na.rm=T) / n()*100),
   #zw09_pin=(survey_total((zw09>=3 & zw09<=4), na.rm=T) / n()*100),
   zw03=(survey_total((zw03>=2 & zw03<=3), na.rm=T) / n()*100),
   zw04_rv=(survey_total((zw04>=1 & zw04<=1), na.rm=T) / n()*100),
   zw04=(survey_total((zw04>=3 & zw04<=4), na.rm=T) / n()*100),
   sc02_pin=(survey_total((sc02>=1 & sc02<=5), na.rm=T) / n()*100)  
  ) %>%
    mutate_at(.,vars(-group_cols()),~ifelse(is.nan(.) | is.infinite(.), NA, .)) %>%
    mutate_at(.,vars(-group_cols()),~replace(., .== 0, NA))
  
  cols_to_remove<- grep("_se", names(df_aggr_pin))
  
  df_aggr_pin<-df_aggr_pin %>% select(-cols_to_remove) %>%
    #reorder variables
    select(sort(names(.))) %>% 
    #move gemeente and metingsjaar to the front
    relocate(any_of(c('GEOITEM', 'PERIOD')), .before=bo01)
  
   
#-----------------------------------------------------------------------------------------------

# Schaalscores

#-----------------------------------------------------------------------------------------------

#schaalscore definitions
#https://www.waarstaatjegemeente.nl/Jive/ViewerReportContents.ashx?report=wsjg_bp_bijlage
 
#rowwise operations
#parallel processing

cluster <- multidplyr::new_cluster(parallel::detectCores() - 2)
multidplyr::cluster_library(cluster, c('tidyverse', 'furrr'))
e <- 10
multidplyr::cluster_copy(cluster, "e")
  
  
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
    
        #Lichamelijke gezondheid
        bep_gez=ifelse(zw01_0==3 | zw01_0==4, 1,
                         ifelse(is.na(zw01_0), NA, 0)),
        
        #Fysiek functioneren
        bep_fys=ifelse(zw01_1==3 | zw01_1==4, 1,
                         ifelse(is.na(zw01_1), NA, 0)),
        
        #Mentale gezondheid
        bep_men=ifelse(zw01_2==3 | zw01_2==4, 1,
                         ifelse(is.na(zw01_2), NA, 0)),
        
        #Taal / cultuur
        bep_cul=ifelse(zw01_3==3 | zw01_3==4, 1,
                         ifelse(is.na(zw01_3), NA, 0)),
        
        #Laag inkomen
        bep_ink=ifelse(zw01_4==3 | zw01_4==4, 1,
                         ifelse(is.na(zw01_4), NA, 0)),
        
        #Uitsluiting
        bep_uit=ifelse(zw01_5==3 | zw01_5==4, 1,
                         ifelse(is.na(zw01_5), NA, 0)),
        
        #Andere beperking
        bep_and=ifelse(zw01_6==3 | zw01_6==4, 1,
                         ifelse(is.na(zw01_6), NA, 0)),
        
        #Mantelzorg overbelasting
        mz_ob=ifelse(zw09==4 | act_mz==1, 1,
                         ifelse(is.na(zw09), NA, 0)),
        
        mb_soc=sum(c(bep_men,bep_cul,bep_ink,bep_uit,mz_ob),na.rm=TRUE),
        
        #Sociale beperking
        bep_soc=ifelse(mb_soc>0, 1,
                         ifelse(is.na(mb_soc), NA, 0)),
        
        bep_soc_neg=ifelse(mb_soc>0, 0,
                         ifelse(is.na(mb_soc), NA, 1)),
        
        #Lichamelijke beperking
        mb_fys=sum(bep_gez,bep_fys,bep_and,na.rm=TRUE),
        
        bep_lich=ifelse(mb_fys>0, 1,
                       ifelse(is.na(mb_fys), NA, 0)),
        
        bep_lich_neg=ifelse(mb_fys>0, 0,
                          ifelse(is.na(mb_fys), NA, 1)),
        
        #meervoudige beperkingen
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
        
        #Vangnet familie, vrienden of buurtgenoten
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
        
        alone_neg=ifelse(zw04<3, 1,
                           ifelse(zw04==5, NA,
                            ifelse(is.na(zw04), NA, 0))),

        
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

bep_dim1_ss=min(c(bep_gez_wei,bep_fys_wei),na.rm=TRUE),
bep_dim2_ss=min(c(bep_men_wei,bep_cul_wei,bep_ink_wei,bep_uit_wei,bep_and_wei),na.rm=TRUE),
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

welzijn_ss=mean(c(ses_ss,socrel_ss,zw02,kunnen_ss,bereid_ss,par_ss,vangnet_ss),na.rm=TRUE),
vitaliteit_ss=mean(c(wl01,verbonden_ss,samenredzaam_ss,safe_ss,fk_ss,voorzieningen_ss,betrouwbaar_ss),na.rm=TRUE),
vitaliteitwelzijn_ss=mean(c(wl01,verbonden_ss,samenredzaam_ss,safe_ss,fk_ss,voorzieningen_ss,betrouwbaar_ss,ses_ss,socrel_ss,zw02,kunnen_ss,bereid_ss,par_ss,vangnet_ss), na.rm=TRUE),  

#_______________________________________________________________________
#maatschappelijke participatie (breedte-maat)

part_score=sum(c(act_ver,act_vw,act_neigh,act_opl,act_arb,act_soc,act_ih,act_hlp), na.rm=TRUE),
#individueel welzijn (breedte-maat)
qol_score=sum(c(health,bep_lich_neg,bep_soc_neg,alone_neg,pleasant_neigh,safeguard), na.rm=TRUE)

#_______________________________________________________________________
  ) %>%
  collect()  %>%
  mutate_at(.,vars(-group_cols()),~ifelse(is.nan(.) | is.infinite(.), NA, .)) %>%
  mutate_at(.,vars(-group_cols()),~replace(., .== 0, NA)) %>% 
  ungroup() %>% 
  select(-GEMEENTE)

#_______________________________________________________________________  
#typology
  
df_ss<-df_ss %>%
  mutate(
    #boundaries are based on all Burgerpeilingen during the last 5 years 
    qol_score_bin=ifelse(qol_score<2.40927414725913, 1,
                        ifelse(qol_score<4.20313840002851, 2,
                               ifelse(qol_score<5.99700265279790, 3,
                                                    ifelse(is.na(qol_score), NA, 4)))),
    
    part_score_bin=ifelse(part_score<1.59205816281436, 1,
                           ifelse(part_score<3.36697460609428, 2,
                                  ifelse(part_score<5.14189104937421, 3,
                                         ifelse(is.na(part_score), NA, 4)))),
    
    typology=ifelse(part_score_bin>2 & qol_score_bin>2 , 1,
                           ifelse(part_score_bin<3 & qol_score_bin>2, 2,
                                  ifelse(part_score_bin>2 & qol_score_bin<3 , 3,
                                         ifelse(part_score_bin<3 & qol_score_bin<3 , 4,
                                              ifelse(is.na(part_score_bin), NA, NA))))),
    
    zorwekkend=ifelse(part_score_bin<2 & qol_score_bin<2 , 1,
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
                             ifelse(is.na(zelfredzaam), NA, NA))
  )


df_ss_weight<- df_ss %>% 
  srvyr::as_survey_design(ids=1, # 1 for no cluster ids 
                          weights=weging, # weight added
                          strata=NULL) # sampling was simple (no strata)


#typolody weerbaarheid
df_ss_vital<-df_ss_weight %>%
group_by(GEOITEM,PERIOD) %>%
  summarize(
    typology_pin1=(survey_total((typology==1), na.rm=T, vartype=vt) / n()*100),
    typology_pin2=(survey_total((typology==2), na.rm=T, vartype=vt) / n()*100),
    typology_pin3=(survey_total((typology==3), na.rm=T, vartype=vt) / n()*100),
    typology_pin4=(survey_total((typology==4), na.rm=T, vartype=vt) / n()*100),
    zorgwekkend_pin1=(survey_total((zorwekkend==1), na.rm=T, vartype=vt) / n()*100)   
)


#_______________________________________________________________________  
#MEAN Schaalscores (gemeente, jaar)

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
#MEAN Schaalscores (gemeente, jaar, typologie)

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
    vitaliteit_ss=survey_mean(vitaliteit_ss,na.rm=TRUE, vartype=vt),
    welzijn_ss=survey_mean(welzijn_ss,na.rm=TRUE, vartype=vt),
    gezond_mean=survey_mean(zw02,na.rm=TRUE, vartype=vt),
    geluk_mean=survey_mean(zw00,na.rm=TRUE, vartype=vt),
    wl01_mean=survey_mean(wl01,na.rm=TRUE, vartype=vt),
    gelukkig_mean=(survey_total((zw00 %in% c(7,8,9,10)), na.rm=T, vartype=vt) / n()*100)
  ) %>%
  pivot_wider(names_from=typology, values_from=-names(.)[1:3],names_sep="b") %>%
  mutate_at(.,vars(-group_cols()),~ifelse(is.nan(.) | is.infinite(.), NA, .)) %>%
  mutate_at(.,vars(-group_cols()),~replace(., .== 0, NA))  


#_______________________________________________________________________  
#MEAN Schaalscores (gemeente, jaar, leeftijd)

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
    gelukkig=(survey_total((zw00 %in% c(7,8,9,10)), na.rm=T, vartype=vt) / n()*100),
    health=survey_mean(zw02,na.rm=TRUE, vartype=vt),
    happy=survey_mean(zw00,na.rm=TRUE, vartype=vt)
  ) %>%
  pivot_wider(names_from=lft_cy, values_from=-names(.)[1:3],names_sep="_cy")  %>%
  mutate_at(.,vars(-group_cols()),~ifelse(is.nan(.) | is.infinite(.), NA, .)) %>%
  mutate_at(.,vars(-group_cols()),~replace(., .== 0, NA))  


#-----------------------------------------------------------------------------------------------

# MERGE

#-----------------------------------------------------------------------------------------------

merged<- Reduce(function(x, y) merge(x, y, all=TRUE), 
                 list(df_aggr_mn,df_aggr_pin,df_ss_aggr,df_ss_type_aggr,df_ss_age_aggr,df_ss_vital))
merged[complete.cases(merged), ]

#keep valid columns
#merged<-merged %>% select(-contains(c("_cy0", "_cyNA", ".NA")))

merged$GEOLEVEL<-"gemeente344"
#report buurten
merged$bp_wijk<-0


#-----------------------------------------------------------------------------------------------

# EXPORT

#-----------------------------------------------------------------------------------------------


out.file<-paste0(output.dir,"/BP.csv")
write.table(merged, file=out.file,quote=TRUE, sep=";", dec = ",", row.names=FALSE)


#----------------------------------------------------------------------------------------------

#Debugging

#----------------------------------------------------------------------------------------------

rlang::last_error()
rlang::last_trace()

end_time<-Sys.time()
end_time - start_time
