# seed data 
library("tidyverse")
seedData <- read_csv("data/seedData.csv", na="-") #note that there is one basket at oxbbow with nothing

#one of the species is labelled wrong
seedData$Species[130] = "PSME"

#remove cones
seedData <- seedData[seedData$Type=="Seed",]

#remove the row that has all NAs
seedData<- seedData %>%
  filter(!is.na(SiteName))

conifers <- c("TSHE", "THPL", "ABsp", "PSME")
decid <- c("ALRU", "ACCI", "ALVI", "ACMA", "ACAM")
urban <- c("Lacamas", "Forest Park", "Forest Park" , "Riverview", "Marquam", "Tryon")
rural <- c("McIver", "Oxbow", "Wildwood", "Sandy", "Barlow")

seedData <- seedData %>% 
  mutate(Urban=case_when(SiteName %in% urban ~ 'urban', SiteName %in% rural ~ 'rural', TRUE ~ NA_character_))

#create a datafile that has the plot IDs
ID <- c("B09" , "B010" , "B42"  , "B011" , "B14" , "B013",  "F45" ,
        "F49" ,  "F43"  , "F47" ,  "F48" , "F44"  , "L22"  , "L41"  ,
        "L12"  , "L33" ,  "L7"  ,  "L5"  ,  "MA35" , "MA30" , "MA031" ,
        "MA029", "MA33" , "MA32",  "MC023","MC078", "MC027", "MC026" ,
        "MC22" , "MC024" , "O038", "O042" , "O041" , "O037" , "O040" ,
        "O038" , "R7" ,  "R11" , "R5"  ,  "R10"  , "R6" ,   "R28" ,
        "S17" ,  "S20" , "S16" ,  "S15"  , "S18",  "S19"  , "T27" ,  
        "T15" ,  "T40" ,  "T13"  , "T37" ,  "T20" ,  "W6" ,  
        "W8"  ,  "W2"  ,  "W3" ,   "W4" ,   "W5")
SiteName <- c("Barlow", "Barlow", "Barlow", "Barlow", "Barlow", "Barlow",
              "Forest Park", "Forest Park", "Forest Park", "Forest Park", "Forest Park", "Forest Park", 
              "Lacamas", "Lacamas", "Lacamas", "Lacamas", "Lacamas", "Lacamas", 
              "Marquam", "Marquam", "Marquam", "Marquam", "Marquam", "Marquam", 
              "McIver", "McIver", "McIver", "McIver", "McIver", "McIver", 
              "Oxbow", "Oxbow", "Oxbow", "Oxbow", "Oxbow", "Oxbow", 
              "Riverview", "Riverview", "Riverview", "Riverview", "Riverview", "Riverview", 
              "Sandy", "Sandy", "Sandy", "Sandy", "Sandy", "Sandy", 
              "Tryon", "Tryon", "Tryon", "Tryon", "Tryon", "Tryon",
              "Wildwood", "Wildwood", "Wildwood", "Wildwood", "Wildwood", "Wildwood")

plotCodes <- tibble(ID, SiteName)


seedData <- seedData %>% 
  mutate(morph=case_when(Species %in% conifers ~ 'con', Species %in% decid ~ 'dec', TRUE ~ NA_character_))

seedData<- seedData %>%
  filter(!is.na(morph))

#only conifers
conSeedData <- seedData[seedData$morph=="con",]
# there are only 45 rows, which means that there are some baskets that did not have any conifer seeds
# need to group all the species together

conSeedSummary <- conSeedData %>% 
  group_by(Urban, SiteName, ID) %>% 
  summarize(a=sum(Number, na.rm=TRUE))

conSeedSummary <- merge(plotCodes, conSeedSummary, by= "ID", all =T)

conSeedSummary <- conSeedSummary %>% 
  mutate(Urban=case_when(SiteName.x %in% urban ~ 'urban', SiteName.x %in% rural ~ 'rural', TRUE ~ NA_character_))

conSeedSummary <- conSeedSummary[, c(1:3, 5)]
conSeedSummary <- conSeedSummary[conSeedSummary$ID != 0 , ]

conSeedSummary <- rename(conSeedSummary, SiteName = SiteName.x)

conSeedSummary$SiteName <- factor(conSeedSummary$SiteName , levels=c(
  "Forest Park", "Lacamas", "Marquam", "Riverview", 
  "Tryon", "Barlow", "McIver", "Oxbow", "Sandy", "Wildwood"))

conSeedSummary$a[is.na(conSeedSummary$a)] = 0

write_csv(conSeedSummary, "data/conSeedSummary.csv")

# plotting this
jpeg("output/conSeedsByForest.jpg")
ggplot(data = conSeedSummary, aes(x = SiteName, y = a, fill = Urban)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab("Number of conifer seeds per basket") + 
  xlab("") +
  ggtitle("Conifer seeds in urban and rural forests")
dev.off()

summary(aov(a ~ Urban / SiteName, data = conSeedSummary))





### if I'm interested in conifer vs decidious

#remove rows 8-12 and anything with an na
decSeedData <- seedData[seedData$morph=="dec",]
decSeedSummary <- decSeedData %>% 
  group_by(Urban, SiteName, ID) %>% 
  summarize(a=sum(Number, na.rm=TRUE))

decSeedSummary <- merge(plotCodes, decSeedSummary, by= "ID", all =T)

decSeedSummary <- decSeedSummary %>% 
  mutate(Urban=case_when(SiteName.x %in% urban ~ 'urban', SiteName.x %in% rural ~ 'rural', TRUE ~ NA_character_))

decSeedSummary <- decSeedSummary[, c(1:3, 5)]
decSeedSummary[is.na(decSeedSummary)] = 0

decSeedSummary$SiteName.x <- factor(decSeedSummary$SiteName.x , levels=c(
  "ForestPark", "Lacamas", "Marquam", "Riverview", 
  "Tryon", "Barlow", "McIver", "Oxbow", "Sandy", "Wildwood"))

decSeedSummary$morph <- c("dec")
conSeedSummary$morph <- c("con")
completeSeedData <- rbind(conSeedSummary, decSeedSummary)


ggplot(data = completeSeedData, aes(x = Urban, y = a, fill=morph)) +
  geom_boxplot() +
  scale_y_continuous(trans='pseudo_log') +
  scale_fill_brewer(palette="Pastel1") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab("Sum of Number of Seeds in a Forest") + 
  xlab(" ") +
  ggtitle("Seeds, type, and forest")

summary(aov(a ~ Urban * morph, data =completeSeedData))
wilcox.test(a ~ Urban, data = decSeedSummary)
