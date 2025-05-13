
#-----------------------------------------------------------------------------------------------

#Preparation

#-----------------------------------------------------------------------------------------------

message("Validation and preparation...")

remove_vars <- function(df, vars_to_remove, keep_only = FALSE) {
  if(keep_only) {
    # Keep only the variables in the list
    matching_vars <- intersect(names(df), vars_to_remove)
    df <- df[, names(df) %in% matching_vars]
  } else {
    # Find which variables in the data frame match the names in the list
    matching_vars <- intersect(names(df), vars_to_remove)
    
    # Remove the matching variables from the data frame
    df <- df[, !(names(df) %in% matching_vars)]
  }
  
  # Return the modified data frame
  return(df)
}

#eliminate previous generated variables (in case of re-run)
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

#add variables (if not there)
add_vars<-c("vz03_5","vz03_6","vz03_7", "wl02_2", "ch06"
            #,"sa01","sa02","sa03"
            )

#research parameters
research_params<-c('gemnr', 'jr', 'weging', 'kw', 'veldwerkmodus', 'wijk', 'org', 'file_id')

#extend valid variables with research parameters
var_vec<-unique(c(var_vec,add_vars,research_params))
  
#get duplicates
#get_dupes(df,-c(id,respondent))

#keep relevant variables and nothing else
#df <- remove_vars(df, var_vec, keep_only = TRUE)

# Keep only relevant variables, sort data frame, place research params at the beginning
df<- df %>%
  #valid vars only
  select(any_of(var_vec)) %>% 
  #reorder variables for readability
  select(sort(names(.))) %>% 
  #research params to the front
  relocate(any_of(research_params), .before=bo01) %>% 
  rename(PERIOD=jr, GEOITEM=gemnr)
