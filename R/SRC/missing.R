
#-----------------------------------------------------------------------------------------------

#Missing

#-----------------------------------------------------------------------------------------------

message("Missing values...")

#missing values analysis
#colSums(is.na(df))

#create missing values for assigned values (77,88,99,-1), including 11 (weet niet/geen mening)
#df[,8:ncol(df)][df[,8:ncol(df)]>10]<- NA
#df[,8:ncol(df)][df[,8:ncol(df)]<= -1]<- NA

# Replace values greater than 10 and less than or equal to -1 with NA for columns 8 to last column
df <- df %>%
  mutate(across(7:ncol(df), ~ifelse(. > 10 | . <= -1, NA, .)))

#missing cat located at 6th position
#recode 6th cat (weet niet/geen mening) to NA for variables with exact 6 cats 
colMax<- function(df) sapply(df, max, na.rm=TRUE)
cols<-ifelse(colMax(df)==6,TRUE,FALSE)

#handle exceptions
#leeftijd and opleiding
get_var_position <- function(df, var_name) {
  var_position <- which(names(df) == var_name)
  return(var_position)
}

#leeftijd
lft_pos<-get_var_position(df, "ch02")

#opleiding
opl_pos<-get_var_position(df, "ch03")

cols[lft_pos]<-FALSE
cols[opl_pos]<-FALSE

df[,cols][df[,cols]== 6]<- NA

#missing cat located at 7th position
ml7<-c('ch06')
#df[,ml7][df[,ml7]==7]<- NA
df <- df %>% mutate(across(all_of(ml7), ~ ifelse(. == 7, NA, .)))

#missing cat located at 5th position
ml5<-c('wl06','zw01_0','zw01_1','zw01_2','zw01_3','zw01_4','zw01_5','zw01_6','zw04', 'zw09')
#df[,ml5][df[,ml5]==5]<- NA
df <- df %>% mutate(across(all_of(ml5), ~ ifelse(. == 5, NA, .)))

#missing cat located at 4th position
ml4<-c('wl14','zw03','zw07','zw10_0','zw10_1','zw10_2')
#df[,ml4][df[,ml4]==4]<- NA
df <- df %>% mutate(across(all_of(ml4), ~ ifelse(. == 4, NA, .)))

#dichotoom
mr_dich<-c("zw13_0","zw13_1","zw13_10","zw13_2","zw13_3","zw13_4",
           "zw13_5","zw13_6","zw13_7","zw13_8","zw13_9",
           "zw08_0","zw08_1","zw08_2","zw08_3",
           "zw08_4","zw08_5","zw08_6","zw08_7")
#df[,mr_dich][df[,mr_dich]>1]<- NA
df <- df %>% mutate(across(all_of(mr_dich), ~ ifelse(. > 1, NA, .)))

#reorder variables for readability
df<- df %>%
  select(sort(names(.))) %>% 
  relocate(any_of(c('gemnr', 'jr', 'weging', 'kw', 'veldwerkmodus', 'wijk', 'org')), .before=bo01) %>% 
  rename(PERIOD=jr, GEOITEM=gemnr)