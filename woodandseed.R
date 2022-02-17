#looking at wood and seed together
library("tidyverse")

trees<-read.csv("skippingR.csv")
wood<-read.csv("wood.csv")

#does seed rain this yr match up to young tree number?
plot(trees$sumyoung~trees$sumseeds, pch=19, xlab="Sum of Conifer Seeds", ylab="Sum of Young Conifers", 
     col=ifelse(trees$urban==0, "forest green", "dark grey")
     , main="This Year's Seed Rain and Trees Under 50 cm Tall")
abline(lm(trees$sumyoung~trees$sumseeds))
summary(lm(trees$sumyoung~trees$sumseeds))

#create a vector that contains the sum of the wood of decay classes 4 and 5
wood45=subset(wood, Decay.class %in% c("4", "5"))
wood45$Forest <- factor(wood45$Forest , levels=c("ForestPark", "Lacamas", "Marquam", "Riverview", 
                                               "Tryon", "Barlow", "McIver", "Oxbow", "Sandy", "Wildwood"))
woodsum<-aggregate(SA~ Forest, data=wood45, sum)
woodsum<-rbind(c("Riverview", 0), woodsum)
woodsum<-rbind(c("ForestPark", 0), woodsum)
#now bind together trees and woodsum
#first need column names and row to match
colnames(woodsum)<-c("site", "SA")
woodsum$site<-rownames(woodsum)<-c("FP", "Riverview", "Lacamas", "Marquam", "Tryon",
                     "Barlow", "McIver", "Oxbow", "Sandy", "Wildwood")


#now actually look at downed wood and young trees
plot(trees$sumyoung~trees$SA, pch=19, xlab="Sum of Downed Wood Decay Classes 4 and 5", ylab="Sum of Young Conifers", main="Young Trees and Downed Wood",
     col=ifelse(trees$urban==0, "forest green", "dark grey"))

#log transform downed wood
trees$SA<-log(trees$SA+1)
plot(trees$sumyoung~trees$SA, pch=19, xlab="Log of Sum of Downed Wood Decay Classes 4 and 5", ylab="Sum of Young Conifers", main="Young Trees and Downed Wood",
     col=ifelse(trees$urban==0, "forest green", "dark grey"))
abline(lm(trees$sumyoung~trees$SA))
summary(lm(trees$sumyoung~trees$SA))

#compare urban and rural log transormed data
boxplot(trees$SA~trees$urban)

