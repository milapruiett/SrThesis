# are there fewer decidious trees?
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

# multiply so that everything is in terms of 30 m^2
#young <- c("g", "s")
#old <- c("sm", "lg", "can")
#tidySpp <- tidySpp %>% 
  #mutate(count=case_when(age %in% young ~ count*6, age %in% old ~ count*1))

conifers <- c("TSHE", "THPL", "AB", "PSME", "TABR")
decid <- c("ALRU", "ACCI", "ALVI", "ACMA", "QUGA")
urban <- c("Lacamas", "ForestPark", "Riverview", "Marquam", "Tryon")
rural <- c("McIver", "Oxbow", "Wildwood", "Sandy", "Barlow")

tidySpp <- tidySpp %>% 
  mutate(Urban=case_when(SiteName %in% urban ~ 'urban', SiteName %in% rural ~ 'rural', TRUE ~ NA_character_))

tidySpp <- tidySpp %>% 
  mutate(morph=case_when(species %in% conifers ~ 'con', species %in% decid ~ 'dec', TRUE ~ NA_character_))

tidySpp <- na.omit(tidySpp) 

write_csv(tidySpp, "data/tidySpp.csv")

jpeg("output/morphAgePseudolog.jpg")
ggplot(data = tidySpp, aes(x = age, y = count, fill=Urban)) + 
  geom_boxplot() + facet_wrap(~ morph, scale="free") + 
  scale_y_continuous(trans='pseudo_log') +
  scale_fill_brewer(palette="Pastel2") +
  theme_light()
dev.off()

jpeg("output/morphAgeLog.jpg")
ggplot(data = tidySpp, aes(x = age, y = count, fill=morph)) + 
  geom_boxplot() + facet_wrap(~ Urban, scale="free") + 
  scale_y_continuous(trans='pseudo_log') +
  scale_fill_brewer(palette="Pastel1") +
  theme_light()
dev.off()

jpeg("output/UrbanFillMorph.jpg")
ggplot(data = tidySpp, aes(x = Urban, y = count, fill=morph)) + 
  geom_boxplot() + facet_wrap(~ age, scale="free") + 
  scale_y_continuous(trans='pseudo_log') +
  scale_fill_brewer(palette="Pastel1") +
  theme_light()
dev.off()

# by site
jpeg("output/MorphAgeBySite.jpg")
ggplot(data = tidySpp, aes(x = age , y = count, fill=morph)) + 
  geom_boxplot() + facet_wrap(~ SiteName , scale="free") + 
  scale_y_continuous(trans='pseudo_log') +
  scale_fill_brewer(palette="Pastel1") +
  theme_light()
dev.off()

summary(aov(count ~ Urban * age * morph, data = tidySpp))


tidyAgeSummary <- tidySpp %>% 
  group_by(morph, SiteName, Urban, age) %>% 
  summarize(sum=sum(count, na.rm=TRUE))

jpeg("output/SumUrbanMorph.jpg")
ggplot(data = tidyAgeSummary, aes(x = Urban, y = sum, fill=morph)) + 
  geom_boxplot() + facet_wrap(~ age, scale="free") + 
  scale_fill_brewer(palette="Pastel1") +
  theme_light()
dev.off()

summary(aov(sum ~ Urban * age * morph, data = tidyAgeSummary))

