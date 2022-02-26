#looking at wood and germs together
library("tidyverse")

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
  summarize(avgG = mean(CONg), seG =std.error(CONg))


wood<-read_csv("wood.csv")

#only want decay classes 4 and 5
wood45=subset(wood, DecayClass %in% c("4", "5"))

#need to add a column to signify if urban or rural
urban <- c("Lacamas", "FP", "RVNA", "Marquam", "Tryon")
rural <- c("McIver", "Oxbow", "Wildwood", "Sandy", "Barlow")

wood45 <- wood45 %>% 
  mutate(Urban=case_when(Forest %in% urban ~ 'urban', Forest %in% rural ~ 'rural', TRUE ~ NA_character_))

#ensure forest is as a factor
wood45$Forest <- factor(wood45$Forest , levels=c(
  "FP", "Lacamas", "Marquam", "RVNA", 
  "Tryon", "Barlow", "McIver", "Oxbow", "Sandy", "Wildwood"))

avgWood <- wood45 %>% 
  group_by(Forest, Urban) %>% 
  summarize(avgW=mean(SA), seW=std.error(SA))

avgWood$Forest<-c("ForestPark", "Lacamas", "Marquam", "Riverview", "Tryon",
                     "Barlow", "McIver", "Oxbow", "Sandy", "Wildwood")
avgWood <- rename(avgWood, SiteName = Forest)

SurveyWood <- inner_join(avgSurvey, avgWood, by="SiteName")

#graph time, this one has confidence limits
ggplot(data = SurveyWood, aes(x = avgW, y = avgG)) +
  stat_smooth(method = lm) +
  geom_point(aes(color = Urban.x)) +
  ylab("No Germinants") +
  xlab("SA of Nurse Log") +
  ggtitle("Nurse Logs and Germinants")

#graph with cross error bars, no line of best fit
ggplot(data = SurveyWood, aes(x = avgW, y = avgG, color = Urban.x, label =SiteName)) +
  geom_point() +
  geom_text(hjust=0, vjust=0) +
  geom_errorbar(aes(ymin = avgG-seG, ymax = avgG+seG), col="grey") + 
  geom_errorbar(aes(xmin = avgW-seW, xmax = avgW+seW), col="grey") +
  scale_color_brewer(palette="Pastel2") +
  theme_light() +
  ylab("No Germinants") +
  xlab("SA of Nurse Log") +
  ggtitle("Nurse Logs and Germinants")

#test the line of best fit
summary(lm(SurveyWood$avgG~SurveyWood$avgW))


