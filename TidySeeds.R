# seed data 
library("plotrix", "psych", "tidyverse", "ggplot2")
seedData <- read_csv("seedData.csv", na="-")
#remove cones
seedData <- seedData[seedData$Type=="Seed",]

conifers <- c("TSHE", "THPL", "ABsp", "PSME")
decid <- c("ALRU", "ACCI", "ALVI", "ACMA")
urban <- c("Lacamas", "Forest Park", "Riverview", "Marquam", "Tryon")
rural <- c("McIver", "Oxbow", "Wildwood", "Sandy", "Barlow")

seedData <- seedData %>% 
  mutate(Urban=case_when(SiteName %in% urban ~ 'urban', SiteName %in% rural ~ 'rural', TRUE ~ NA_character_))

seedData <- seedData %>% 
  mutate(morph=case_when(Species %in% conifers ~ 'con', Species %in% decid ~ 'dec', TRUE ~ NA_character_))

#remove rows 8-12 and anything with an na
seedData <- seedData[, c(1:7, 13)]
seedData <- na.omit(seedData) 

seedSummary <- seedData %>% 
  group_by(Species, SiteName, Type) %>% 
  summarize(a=mean(Number, na.rm=TRUE), se=std.error(Number, na.rm=TRUE))


seedConD <- seedData %>% 
  group_by(morph, Urban) %>% 
  summarize(a=sum(Number, na.rm=TRUE), se=std.error(Number, na.rm=TRUE))
seedConD <- seedConD[1:4,]

#boxplot with the difference between urban and rural seed rain and morphology
ggplot(data = seedData, aes(x = Urban, y = Number, color=morph)) + geom_boxplot()  #main graph


# two-way anova test
summary(aov(Number ~ Urban + morph, data= seedData))


