
#-----------------------------------------------------------------------------------------------

#Preparation

#-----------------------------------------------------------------------------------------------

message("Validation and preperation...")

#eliminate previous generated variables (in case of re-run)
remove_vars <- function(df, vars_to_remove) {
  # Find which variables in the data frame match the names in the list
  matching_vars <- intersect(names(df), vars_to_remove)
  
  # Remove the matching variables from the data frame
  df <- df[, !(names(df) %in% matching_vars)]
  
  # Return the modified data frame
  return(df)
}

df <- remove_vars(df, c("GEOITEM", "PERIOD"))

#check data structure
var_nms<-colnames((df))
var_len<-length(var_nms)

var.loc<-here::here("DATA/REF/var_df.RData")
#var_df<-as_tibble(var_nms)
#save(var_df,file="DATA/REF/var_df.RData")

#vector with valid variables
load(var.loc)

var_vec<-as.vector(var_df$value)

#add variables 
add_vars<-c("vz03_5","vz03_6")

var_vec<-c(var_vec,add_vars)
  
#get duplicates
#get_dupes(df,-c(id,respondent))