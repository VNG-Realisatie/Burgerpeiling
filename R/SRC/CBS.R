
#-----------------------------------------------------------------------------------------------

#CBS API

#-----------------------------------------------------------------------------------------------

toc <- cbs_get_toc("Language" = "nl") %>% 
  filter(grepl('gemeente', ShortDescription))
View(toc)

cbs.datasets <- list(
  # Gebieden in Nederland 
  "Gebieden" = "85067NED",
  "Kerncijfers" = "85318NED"
) 


for (cbs.id in cbs.datasets){
  path.cbs.item <- file.path(cbs.dir,cbs.id)
  cbs_download_table(id=cbs.id, dir = path.cbs.item, typed = TRUE, cache = TRUE, verbose = FALSE)
  print(cbs.id)
}


DataCleansing <- function(x){
  x <- str_trim(x)  
  return(x)
}

for (name in names(cbs.datasets)){
  # Bestandslocatie, elk bestand heet data.csv
  path.cbs.data <- file.path(cbs.dir,cbs.datasets[name],"data.csv")
  # CSV bestanden inlezen, maakt dataframes met opgegeven naam.
  assign(name, 
         read_csv(file = path.cbs.data, 
                  #col_types = cols(.default = "c"), 
                  locale = readr::locale(encoding = "windows-1252")) # alle kolommen als tekst laden, daarna bewerken
  )
  # CSV bestanden met metadata importeren, maakt dataframes met suffix meta.
  path.cbs.meta <- file.path(cbs.dir,cbs.datasets[name],"DataProperties.csv")
  assign(paste(name,'meta',sep = '_'), 
         read_csv(file = path.cbs.meta, 
                  #col_types = cols(.default = "c"), 
                  locale = readr::locale(encoding = "windows-1252")) # alle kolommen als tekst laden, daarna bewerken
  )
  assign(name,
         get(name) %>% mutate_all(DataCleansing)) # spaties links en rechts verwijderen
  print(name)
}

# Metadata bekijken.
View(Kerncijfers_meta)

cbs_codes<-Gebieden %>%
  #Gebieden %>%
  select(Code_1,Naam_2) %>%
  rename(
    Gemeentecode = Code_1,
    Gemeentenaam = Naam_2
  )
  
cbs_codes$Gemeentecode<-as.numeric(gsub("[[:alpha:]]", "", cbs_codes$Gemeentecode))
  
file.cbs.nme<-here::here(cbs.dir,"cbs_munic_codes.xlsx")

openxlsx::write.xlsx(cbs_codes, file.cbs.nme, colNames = TRUE)