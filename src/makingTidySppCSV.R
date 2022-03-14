# making tidySpp file
library("tidyverse")

prelim<-read_csv("data/seedling.csv") 
prelim$SiteName <- factor(prelim$SiteName , levels=c("Barlow", 
                                                     "McIver", "Oxbow", "Sandy", "Wildwood", 
                                                     "ForestPark", "Lacamas", "Marquam", "Riverview", 
                                                     "Tryon"))
tidySpp<- prelim %>% 
  gather(key = "type", value="count", TSHEg:QUGAcan)

# separate age and sp, ensure age is in right order
tidySpp<-tidySpp %>% extract(type, c("species", "age"), "([A-Z]+)([a-z]+)")
tidySpp$age <- factor(tidySpp$age, levels = c("g", "s", "sm", "lg", "can"))

conifers <- c("TSHE", "THPL", "AB", "PSME")
decid <- c("ALRU", "ACCI", "ACMA", "QUGA")
urban <- c("Lacamas", "ForestPark", "Riverview", "Marquam", "Tryon")
rural <- c("McIver", "Oxbow", "Wildwood", "Sandy", "Barlow")

tidySpp <- tidySpp %>% 
  mutate(Urban=case_when(SiteName %in% urban ~ 'urban', SiteName %in% rural ~ 'rural', TRUE ~ NA_character_))

tidySpp <- tidySpp %>% 
  mutate(morph=case_when(species %in% conifers ~ 'con', species %in% decid ~ 'dec', TRUE ~ NA_character_))

tidySpp <- na.omit(tidySpp) 

write_csv(tidySpp, "data/tidySpp.csv")