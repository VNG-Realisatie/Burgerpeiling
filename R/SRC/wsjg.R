
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
#if (any(is.na(df_export[, typology_check])) || any(df_export[, typology_check] >= 80)) {
#  df_export[, typology_check] <- NA
#}


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


#-------------------------------------------------------------------------------------
#report suspicious records 
identify_outliers <- function(data) {
  # Replace -99998 with NA
  data[data == -99998] <- NA
  
  if (nrow(data) == 0) {
    stop("The dataset is empty.")
  }
  
  num_outliers <- numeric(nrow(data))
  
  for (i in 1:nrow(data)) {
    record <- unlist(data[i, 3:ncol(data), drop = FALSE])  # Extract columns starting from the third column
    record <- as.numeric(record)
    record <- na.omit(record)
    if (length(record) == 0) {
      num_outliers[i] <- 0
    } else {
      q1 <- quantile(record, 0.25, na.rm = TRUE)
      q3 <- quantile(record, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
      num_outliers[i] <- sum(record < lower_bound | record > upper_bound, na.rm = TRUE)
    }
  }
  
  max_outliers <- max(num_outliers)
  records_with_max_outliers <- which(num_outliers == max_outliers)
  
  if (length(records_with_max_outliers) == 0) {
    stop("No records with outliers found.")
  }
  
  # Return the first two columns for records with the most outliers
  return(data[records_with_max_outliers, 1:401, drop = FALSE])
}



# Identify records with the most 95% interval outliers
suspect_outlier <- identify_outliers(df_export)

# Print the record(s) with the most outliers and the number of outliers
cat("Gemeente record(s) with the most outliers:", suspect_outlier[[1]], "\n")
cat("CBS ID(s) of outliers:", suspect_outlier[[2]], "\n")
