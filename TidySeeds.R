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

#boxplot with the difference between urban and rural seed rain and morphology
ggplot(data = seedData, aes(x = SiteName, y = Number, fill=morph)) +
  geom_boxplot() +
  scale_y_continuous(trans='pseudo_log') +
  scale_fill_brewer(palette="Pastel1") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab("Number of Seeds") + 
  xlab(" ") +
  ggtitle("Seeds, type, and forest")

ggplot(data = seedData, aes(x = Urban, y = Number, fill=morph)) + 
  geom_boxplot() +
  scale_y_continuous(trans='log10')+
  scale_fill_brewer(palette="Pastel1") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab("Seeds per sq m sampled") + 
  xlab(" ") +
  ggtitle("Seed Rain in Urban and Rural Forests")

ggplot(data = seedData, aes(x = morph, y = Number, fill=Urban)) + 
  geom_boxplot() +
  scale_y_continuous(trans='log10')+
  scale_fill_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab("Seeds per sq m sampled") + 
  xlab(" ") +
  ggtitle("Seed Rain in Urban and Rural Forests")

# two-way anova test
summary(aov(Number ~ Urban + morph, data= seedData))

#summarize data
seedSummary <- seedData %>% 
  group_by(morph, SiteName, Urban) %>% 
  summarize(a=sum(Number, na.rm=TRUE))

ggplot(data = seedSummary, aes(x = Urban, y = a, fill=morph)) +
  geom_boxplot() +
  scale_y_continuous(trans='log10') +
  scale_fill_brewer(palette="Pastel1") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab("Sum of Number of Seeds in a Forest") + 
  xlab(" ") +
  ggtitle("Seeds, type, and forest")

#t.test, with a hist first to show why I log transformed the data
ggplot(data=seedData, aes(x =Number, fill = morph )) + 
  facet_wrap(~Urban) +
  geom_histogram( color="#e9ecef") +
  scale_fill_brewer(palette="Pastel1") +
  theme_light()

seedData$lNo <- log10(1+seedData$Number)
summary(aov(seedData$lNo ~ seedData$Urban + seedData$morph))

t.test(log10(1+seedSummary$a)~seedSummary$Urban)
summary(aov(seedSummary$a ~ seedSummary$Urban + seedSummary$morph))
conSeed <- seedSummary[1:10,]
t.test(conSeed$a ~ conSeed$Urban)
decSeed <- seedSummary[11:20,]
t.test(decSeed$a ~ decSeed$Urban)
