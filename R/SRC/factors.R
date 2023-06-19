
#-----------------------------------------------------------------------------------------------

#Factors

#-----------------------------------------------------------------------------------------------

message("Factors and levels...")

#read municipality names 
file_type<-'xlsx'
qry<-paste0("*",file_type)
files<- fs::dir_ls(glob=qry, path=cbs.dir)

#read xls-sheets 
gemeenten_meta<- map_df(set_names(files), function(file) {
  file %>% 
    map_df(
      ~ readxl::read_xlsx(path=file, col_names=TRUE, sheet=1, trim_ws=TRUE, guess_max=20))
})

gemeenten_meta$Gemeentecode<-as.numeric(gemeenten_meta$Gemeentecode)

#create vectors 
#numeric
gem_levels<-gemeenten_meta$Gemeentecode
#string
gem_labels<-gemeenten_meta$Gemeentenaam

#create factor
df$GEMEENTE<- factor(df$GEOITEM, levels=gem_levels, labels=gem_labels)

#position at beginning of data frame
df<-df %>%
  relocate(any_of(c('GEMEENTE')), .before=GEOITEM) %>%
#filter municipalities according to recent CBS definition
 filter(GEOITEM %in% gem_levels)