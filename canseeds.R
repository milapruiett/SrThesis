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
  group_by(Urban, SiteName) %>% 
  summarize(meanCan=mean(sum, na.rm=TRUE), seCan=std.error(sum, na.rm=TRUE))

# inner join
canSeed= conSeedSite %>% inner_join(conCanSite,by="SiteName")

# linear model


# plot
ggplot(data = canSeed, aes(x=meanCan, y=meanSeed)) + geom_point()



