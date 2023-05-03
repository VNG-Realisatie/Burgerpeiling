
#-----------------------------------------------------------------------------------------------

#WSJG

#-----------------------------------------------------------------------------------------------

message("WSJG...")

#VNG use only...

df_export <- df_munic

#report features with values above 99
df_export %>% 
  transmute_if(~ any(.x > 99), I)

#check na's in typology
typology_check<-c("typology_pin1","typology_pin2","typology_pin3","typology_pin4")
which(is.na(df_export[,typology_check]), arr.ind=TRUE)

#remove records that failed typology
#df_export<-df_export %>%
#  drop_na(all_of(typology_check))

#-----------------------------------------------------------------------------------------------

#specific notation for WSJG

#missing values
df_export <- df_export %>% replace(is.na(.), -99998)

#geolevel
df_export$GEOLEVEL<-"gemeente344"

#report buurten
df_export$bp_wijk<-0

#remove temporary variables
df_export$GEOYR<-NULL