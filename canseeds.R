# do the no of canopy trees predict the no of seeds?

#need a table that has the avg no seeds/can and se
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

#summarize data
conSummary <- seedData %>% 
  filter(morph == "con", na.rm = TRUE) %>% 
  group_by(morph, SiteName, Urban) %>% 
  summarize(avgSeed=mean(Number, na.rm=TRUE), seSeed =std.error(Number, na.rm=TRUE))

survey<-read_csv("seedling.csv") 
survey$SiteName <- factor(survey$SiteName , levels=c("Barlow", 
                                                     "McIver", "Oxbow", "Sandy", "Wildwood", 
                                                     "ForestPark", "Lacamas", "Marquam", "Riverview", 
                                                     "Tryon"))
urban <- c("Lacamas", "ForestPark", "Riverview", "Marquam", "Tryon")
rural <- c("McIver", "Oxbow", "Wildwood", "Sandy", "Barlow")

survey <- survey %>% 
  mutate(Urban=case_when(SiteName %in% urban ~ 'urban', SiteName %in% rural ~ 'rural', TRUE ~ NA_character_))
survey$Urban <- as.factor(survey$Urban)

avgSurvey <- survey %>% 
  group_by(SiteName, Urban) %>% 
  summarize(avgCAN = mean(CONcan), seCAN =std.error(CONcan))

#combine tables
surveyCan <- inner_join(avgSurvey, conSummary, by="SiteName")

#graph time, this one has confidence limits
ggplot(data = surveyCan, aes(x = avgCAN, y = avgSeed)) +
  stat_smooth(method = lm) +
  geom_point(aes(color = Urban.x)) +
  ylab("No Seeds per Basket") +
  xlab("No Canopy Trees per Plot") +
  ggtitle("Increasing canopy trees increases seed rain")

#graph with cross error bars, no line of best fit
ggplot(data = surveyCan, aes(x = avgCAN, y = avgSeed, color = Urban.x, label =SiteName)) +
  geom_point() +
  geom_text(hjust=0, vjust=0) +
  geom_errorbar(aes(ymin = avgSeed-  seSeed, ymax = avgSeed + seSeed), col="grey") + 
  geom_errorbar(aes(xmin = avgCAN - seCAN, xmax = avgCAN + seCAN), col="grey") +
  scale_color_brewer(palette="Pastel2") +
  theme_light() +
  ylab("No Seeds per Basket") +
  xlab("No Canopy Trees per Plot") +
  ggtitle("Conifer canopy trees and conifer seeds")

#test the line of best fit
summary(lm(surveyCan$avgSeed~surveyCan$))





