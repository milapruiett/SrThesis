# do the no of canopy trees predict the no of seeds?
library("plotrix", "tidyverse")
detach(package:plyr)
conSeedSummary <- read_csv("conSeedSummary.csv")
tidySpp <- read_csv("tidySpp.csv")

#summary by site of the mean and SE
conSeedSite <- conSeedSummary %>% 
  group_by(Urban, SiteName) %>% 
  summarize(meanSeed=mean(a, na.rm=TRUE), seSeed=std.error(a, na.rm=TRUE))

can <- tidySpp %>% filter(morph == "con", age == "can")

conCanSummary <- can %>% 
  group_by(Urban, SiteName, PlotID) %>% 
  summarize(sum=sum(count, na.rm=TRUE))

count(conCanSummary, "SiteName")

# add the missing plots with zeroes
conCanSummary <- conCanSummary %>% ungroup() 
conCanSummary <- conCanSummary %>% add_row (Urban = "urban", SiteName = "Lacamas", PlotID= 60, sum =0) %>% 
  add_row (Urban = "urban", SiteName = "Riverview", PlotID= 200, sum =0) %>% 
  add_row (Urban = "urban", SiteName = "Riverview", PlotID= 201, sum =0)

# get the mean and standard error of canopy conifers into one table 
conCanSite <- conCanSummary %>% 
  group_by(SiteName) %>% 
  summarize(meanCan=mean(sum, na.rm=TRUE), seCan=std.error(sum, na.rm=TRUE))

# inner join
canSeed= conSeedSite %>% inner_join(conCanSite,by="SiteName")

# linear model
summary(lm(canSeed$meanSeed ~ canSeed$meanCan))

# plot
# as a line
ggplot(data = canSeed, aes(x=meanCan, y=meanSeed, label= SiteName)) + 
  geom_point(aes(color = Urban)) +
  geom_smooth(method=lm) +
  scale_color_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab("Average conifer seeds per basket") + 
  xlab("Average conifer canopy trees per transect") +
  ggtitle("Conifer canopy trees and seeds")

# with cross erros
ggplot(data = canSeed, aes(x = meanCan, y = meanSeed, color = Urban, label =SiteName)) +
  geom_point() +
  geom_text(hjust=0, vjust=0) +
  geom_errorbar(aes(ymin = meanSeed - seSeed, ymax = meanSeed + seSeed), col="grey") + 
  geom_errorbar(aes(xmin = meanCan - seCan, xmax = meanCan + seCan), col="grey") +
  scale_color_brewer(palette="Pastel2") +
  theme_light() +
  ylab("No Seeds per Basket") +
  xlab("No Canopy Trees per Plot") +
  ggtitle("Conifer canopy trees and conifer seeds")



