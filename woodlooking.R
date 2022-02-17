#load tidvyverse
library(tidyverse)

#basic looking at downed wood
wood<-read.csv("wood.csv") 
plot(wood$SA [wood$AvgDepth<4] ~ wood$Forest[wood$AvgDepth<4])

#can't help myself: two way anova looking at forest and forest type
two.way <- aov(wood$SA[wood$Decay.class==5] ~ wood$Urban[wood$Decay.class==5] + wood$Forest[wood$Decay.class==5])
summary(two.way)


#average wood of a decay class 3 in each forest
tapply(wood$SA[wood$Decay.class>4], wood$Forest[wood$Decay.class>4], mean)

#wood that is 4 and higher seems like the best metric to me
#subset also rearrange the order of the parks so urban / rural are clumped
wood4=subset(wood, Decay.class %in% c("4", "5"))
wood4$Forest <- factor(wood4$Forest , levels=c("ForestPark", "Lacamas", "Marquam", "Riverview", "Tryon", "Barlow", "McIver", "Oxbow", "Sandy", "Wildwood"))
plot(wood4$SA~wood4$Forest, xlab="Forest Name", ylab="Log Surface Area (cm^2)", main="All Logs Decay Class 4 and Higher")


#test with two-way anova
two.way <- aov(wood4$SA~ wood4$Urban + wood4$Forest)
summary(two.way)

#combine urban sites together and rural together
woodcombo<-aggregate(SA~ Forest, data=wood4, mean) #aggregate makes a dataframe
#because Riverveiw and FP have no wood decay class 4 or higher, need to add new rows with 0s
woodcombo<-rbind(c("Riverview", 0), woodcombo)
woodcombo<-rbind(c("ForestPark", 0), woodcombo)
#need to signfify if urban or rural
woodcombo$Urban<-c(1,1,1,1,1,0,0,0,0,0)
#boxplot with the means
boxplot(SA ~ Urban, woodcombo) #this isn't working so lets see if its numeric
is.numeric(woodcombo$SA) #SA is not numeric so lets change that
woodcombo$SA <-as.numeric(woodcombo$SA) #yay now it works
boxplot(SA ~ Urban, woodcombo, main="Downed Wood Decay States 4 and 5") 

#to do just 5
#wood that is 4 and higher seems like the best metric to me
#subset also rearrange the order of the parks so urban / rural are clumped
wood5=subset(wood, Decay.class %in% c("4", "5"))
wood5$Forest <- factor(wood5$Forest , levels=c("ForestPark", "Lacamas", "Marquam", "Riverview", "Tryon", "Barlow", "McIver", "Oxbow", "Sandy", "Wildwood"))
plot(wood4$SA~wood4$Forest, xlab="Forest Name", ylab="Log Surface Area (cm^2)", main="All Logs Decay Class 4 and Higher")

#combine urban sites together and rural together
woodcombo2<-aggregate(SA~ Forest, data=wood5, mean) #aggregate makes a dataframe
#because Riverveiw and FP have no wood decay class 4 or higher, need to add new rows with 0s
woodcombo2<-rbind(c("Riverview", 0), woodcombo2)
woodcombo2<-rbind(c("ForestPark", 0), woodcombo2)
woodcombo2<-rbind(c("Marquam", 0), woodcombo2)
woodcombo2<-rbind(c("Oxbow", 0), woodcombo2)
woodcombo2<-rbind(c("Wildwood", 0), woodcombo2)
#need to signfify if urban or rural
woodcombo2$Urban<-c(0,0,1,1,1,1,1,0,0,0)
#boxplot with the means
boxplot(SA ~ Urban, woodcombo2) #this isn't working so lets see if its numeric
is.numeric(woodcombo2$SA) #SA is not numeric so lets change that
woodcombo2$SA <-as.numeric(woodcombo2$SA) #yay now it works
boxplot(SA ~ Urban, woodcombo2, main="Downed Wood Decay State 5") 
wilcox.test(woodcombo2$SA~woodcombo2$Urban)


wood5=subset(wood, Decay.class %in% c("5"))
plot(wood5$SA~wood5$Forest, xlab="Forest Name", ylab="Log Surface Area (cm^2)", main="All Logs Decay Class 5")
tapply(wood5$SA, wood5$Forest, mean)

two.way <- aov(wood5$SA~ wood5$Urban + wood5$Forest)
summary(two.way)
