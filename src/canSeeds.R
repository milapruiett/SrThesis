# do the no of canopy trees predict the no of seeds?
library("plotrix", "tidyverse", "smatr")
detach(package:plyr)
conSeedSummary <- read_csv("data/conSeedSummary.csv")
tidySpp <- read_csv("data/tidySpp.csv")

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

# get by m^2
canSeed$meanSeed <- 10000/145.5 * canSeed$meanSeed
canSeed$meanCan <- 1/30 * canSeed$meanCan
canSeed$seSeed <- 10000/145.5 * canSeed$seSeed
canSeed$seCan <- 1/30 * canSeed$seCan

# plot 
# with cross erros
jpeg("output/canSeedByForestCrossErrors.jpg")
ggplot(data = canSeed, aes(x =meanCan, y = meanSeed, color = Urban)) +
  geom_errorbar(aes(ymin = meanSeed - seSeed, ymax = meanSeed + seSeed), col="grey") + 
  geom_errorbar(aes(xmin = meanCan - seCan, xmax = meanCan + seCan), col="grey") +
  geom_point(size = 4) +
  geom_text(aes(label = SiteName), colour="black", size = 3)+
  scale_color_brewer(palette="Pastel2") +
  theme_light() +
  theme(text=element_text(size=15)) +
  ylab(bquote("No seeds per m" ^2))+
  xlab(bquote("No canopy trees per m" ^2)) +
  ggtitle("Conifer canopy trees and conifer seeds")
dev.off()


# sma
test <- sma(meanSeed ~ meanCan, data = canSeed, slope.test = 0)
summary(test)

# plotting sma results
jpeg("output/canSeedByForestLine.jpg")
ggplot(data = canSeed, aes(x = meanCan, y = meanSeed, color = Urban)) +
  geom_point(size = 4, aes(color = Urban)) +
  geom_abline(intercept = -318 , slope = 50611) +
  scale_color_brewer(palette="Pastel2") +
  geom_text(aes(label = SiteName), colour="black", size = 3)+
  theme_light() +
  theme(legend.title = element_blank()) +
  theme(text=element_text(size=15)) +
  ylab(bquote("No seeds per m" ^2)) +
  xlab(bquote("No canopy trees per m" ^2)) +
  ggtitle("Conifer canopy trees and seeds")
dev.off()

# if you want to show the quartiles
# geom_abline(intercept = -908 , slope = 25260, linetype = "dotted") +
# geom_abline(intercept = 270 , slope = 101401, linetype = "dotted") +
