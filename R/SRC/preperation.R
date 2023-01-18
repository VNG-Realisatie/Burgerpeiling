
#-----------------------------------------------------------------------------------------------

#Preperation

#-----------------------------------------------------------------------------------------------

#eliminate previous generated variables (in case of re-run)
df$GEOITEM<-NULL
df$PERIOD<-NULL

#check data structure
var_nms<-colnames((df))
var_len<-length(var_nms)

var.loc<-here::here("DATA/REF/var_df.RData")
#var_df<-as_tibble(var_nms)
#save(var_df,file="DATA/REF/var_df.RData")

#vector with valid variables
load(var.loc)
var_vec<-as.vector(var_df)

#get duplicates
#get_dupes(df,-c(id,respondent))