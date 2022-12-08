
#-----------------------------------------------------------------------------------------------

#Missing

#-----------------------------------------------------------------------------------------------

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
ml5<-c('wl06','zw01_0','zw01_1','zw01_2','zw01_3','zw01_4','zw01_5','zw01_6','zw04', 'zw09')
df[,ml5][df[,ml5]==5]<- NA

#missing cat located at 4th position
ml4<-c('wl14','zw03','zw07','zw10_0','zw10_1','zw10_2')
df[,ml4][df[,ml4]==4]<- NA

#reorder variables for readability
df<- df %>%
  select(sort(names(.))) %>% 
  relocate(any_of(c('gemnr', 'jr', 'weging', 'kw', 'veldwerkmodus', 'wijk', 'org')), .before=bo01) %>% 
  rename(PERIOD=jr, GEOITEM=gemnr)
