
#-----------------------------------------------------------------------------------------------

#WSJG

#-----------------------------------------------------------------------------------------------

message("WSJG...")

#VNG use only...
df_export <- df_munic

# Report features with values above 99
features_above_99 <- df_export %>%
  select_if(~ any(. > 99))

# Check NA's in typology
typology_check <- c("typology_pin1", "typology_pin2", "typology_pin3", "typology_pin4")
na_typology_rows <- which(is.na(df_export[, typology_check]), arr.ind = TRUE)

# Check for NA or values above 80% in any column
if (any(is.na(df_export[, typology_check])) || any(df_export[, typology_check] >= 80)) {
  df_export[, typology_check] <- NA
}


#remove records that failed typology
#df_export<-df_export %>%
#  drop_na(all_of(typology_check))

#-----------------------------------------------------------------------------------------------

#specific notation for WSJG

#missing values
df_export <- df_export %>% replace(is.na(.), -99998)

#geolevel
df_export$GEOLEVEL<-"gemeente"

#report buurten
df_export$bp_wijk<-0

#remove temporary variables
if ("GEOYR" %in% names(df_export)) {
  df_export$GEOYR <- NULL
}
