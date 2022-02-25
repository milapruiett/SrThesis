# are there fewer decidious trees?
prelim<-read.csv("seedling.csv") 
prelim$SiteName <- factor(prelim$SiteName , levels=c("Barlow", 
                                                     "McIver", "Oxbow", "Sandy", "Wildwood", 
                                                     "ForestPark", "Lacamas", "Marquam", "Riverview", 
                                                     "Tryon"))
tidySpp<- prelim %>% 
  gather(key = "type", value="count", TSHEg:QUGAcan)

# separate age and sp, ensure age is in right order
tidySpp<-tidySpp %>% extract(type, c("species", "age"), "([A-Z]+)([a-z]+)")
tidySpp$age <- factor(tidySpp$age, levels = c("g", "s", "sm", "lg", "can"))

# multiply so that everything is in terms of 30 m^2
young <- c("g", "s")
old <- c("sm", "lg", "can")
tidySpp <- tidySpp %>% 
  mutate(count=case_when(age %in% young ~ count*6, age %in% old ~ count*1))

conifers <- c("TSHE", "THPL", "ABsp", "PSME")
decid <- c("ALRU", "ACCI", "ALVI", "ACMA")
urban <- c("Lacamas", "ForestPark", "Riverview", "Marquam", "Tryon")
rural <- c("McIver", "Oxbow", "Wildwood", "Sandy", "Barlow")

tidySpp <- tidySpp %>% 
  mutate(Urban=case_when(SiteName %in% urban ~ 'urban', SiteName %in% rural ~ 'rural', TRUE ~ NA_character_))

tidySpp <- tidySpp %>% 
  mutate(morph=case_when(species %in% conifers ~ 'con', species %in% decid ~ 'dec', TRUE ~ NA_character_))

tidySpp <- na.omit(tidySpp) 

ggplot(data = tidySpp, aes(x = age, y = count, fill=Urban)) + geom_boxplot() + facet_wrap(~ morph, scale="free") + scale_y_continuous(trans='pseudo_log')

ggplot(data = tidySpp, aes(x = age, y = count, fill=morph)) + geom_boxplot() + facet_wrap(~ Urban, scale="free") + scale_y_continuous(trans='log10')

ggplot(data = tidySpp, aes(x = Urban, y = count, fill=morph)) + geom_boxplot() + facet_wrap(~ age, scale="free") + scale_y_continuous(trans='log10')
