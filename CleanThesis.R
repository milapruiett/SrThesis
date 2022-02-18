#cleaned up document
# get necessary packages 
library("vegan")
library("tidyverse")

#####################################################################
### seedling data, which has each plot within each site separated out
### to make graphs that show sites individually
## read in and format data
prelim<-read.csv("seedling.csv") 
prelim$SiteName <- factor(prelim$SiteName , levels=c("Barlow", 
                  "McIver", "Oxbow", "Sandy", "Wildwood", 
                  "ForestPark", "Lacamas", "Marquam", "Riverview", 
                  "Tryon"))
germplot=plot(prelim$CONg~prelim$SiteName, 
              xlab = "ANOVA p=0.0005", ylab="No. Conifer Germinants in 0.5 sq cm", 
              main = "Rural forests have more conifer germinants than urban forests",
              col=c("#B8D685",
                    "#B8D685", "#B8D685", "#B8D685", "#B8D685", 
                    "#FFCA85", "#FFCA85","#FFCA85", 
                    "#FFCA85", "#FFCA85"))
l=legend(x= "topright", legend=c("Rural", "Urban"), 
         fill=c("#B8D685", "#FFCA85"))

# how to analyze the seedling data
summary(aov(prelim$CONg~ prelim$Urban))

###########################################################################
### the sum of each age class for each site, to compare urban and rural
## read in and format the data
sumdata<- read.csv("skippingR.csv") 
sumdata$urban[sumdata$urban==1] <- "Urban"
sumdata$urban[sumdata$urban==0] <- "Rural"
sumdata$urban<-as.factor(sumdata$urban)

## create graphs that show just one age group
# multiply so that the number of seeds is per square meter (10,000/925)
boxplot((10000/925)*sumdata$sumseeds ~sumdata$urban, main="Rural forests have more conifer seed rain than urban forests",
        ylab="Conifer seeds per sq m sampled", 
        xlab="Wilcoxon test, p=0.05",
        col=c("#B8D685", "#FFCA85"))
# analyze using wilcox test
wilcox.test(sumdata$sumseeds~sumdata$urban)
# log-transform the seed data
hist(log(1+sumdata$sumseeds)) #more normal dist
hist(sumdata$sumseeds) #not normal dist
t.test(log(1+sumdata$sumseeds) ~sumdata$urban) # t-test bc normal dist
tapply(sumdata$sumg, sumdata$urban, mean) #how to get the mean of a group


###########################################################################
### downed wood data
wood<-read.csv("wood.csv")
wood$Urban[wood$Urban==1] <- "Urban"
wood$Urban[wood$Urban==0] <- "Rural"
# to get each site individually
wood45=subset(wood, Decay.class %in% c("4", "5"))
wood45$Forest <- factor(wood45$Forest , levels=c(
  "ForestPark", "Lacamas", "Marquam", "Riverview", 
  "Tryon", "Barlow", "McIver", "Oxbow", "Sandy", "Wildwood"))
# to get the sum of wood in each forest to compare urban and rural more broadly
woodcombo2<-aggregate(SA~ Forest, data=wood45, mean) #aggregate makes a dataframe
#because Riverveiw and FP have no wood decay class 4 or higher, need to add new rows with 0s
woodcombo2<-rbind(c("Riverview", 0), woodcombo2)
woodcombo2<-rbind(c("ForestPark", 0), woodcombo2)
# SA needs to be numeric
woodcombo2$SA <-as.numeric(woodcombo2$SA)
# need to add a column to signify if urban or rural
woodcombo2$Urban<-c("Urban", "Urban", "Urban", "Urban", "Urban", "Rural",
                    "Rural", "Rural", "Rural", "Rural")
#now can actually graph and look at them
boxplot(.001*woodcombo2$SA~woodcombo2$Urban,
        main="Rural forests generally have more decayed wood",
        xlab="Wilcoxon test, p=0.14",
        col=c("#B8D685", "#FFCA85"),
        ylab="Total decayed wood in sq m per forest"
        ) 
# want to log transform the sa?
woodcombo2$lSA<-log10(1+(1/1500)*woodcombo2$SA)
t.test(log10(1+(1/1500)*woodcombo2$SA)~woodcombo2$Urban)
#what about just graphing it with a log transformed axis?
boxplot(I(1+(1/1500)*woodcombo2$SA)~woodcombo2$Urban, log="y",
        col=c("#B8D685", "#FFCA85"),
        ylab="Sq cm nurse log per sq m sampled", 
        main="Rural forests generally have more decayed wood",
        xlab="T-test, p=0.11") 

#################################################################################
###combining nurse log data and conifer sum data
names(sumdata)[1]<-"Forest"
woodcombo2$Forest<-c("FP", "Riverview", "Lacamas", "Marquam", "Tryon",
"Barlow", "McIver", "Oxbow", "Sandy", "Wildwood")
sumdata<-merge(woodcombo2, sumdata, by="Forest")
sumdata$SA<-as.numeric(sumdata$SA) #SA not numeric for some reason

##does seed rain this yr match up to germ tree number?
plot(trees$sumseeds~trees$sumcan, pch=19, ylab="Sum of Conifer Seeds", 
     xlim=c(-1, 13), cex=3
      ,xlab="Sum of Canopy Conifers", 
     col=ifelse(trees$urban==0, "#B8D685", "#FFCA85")
     , main="This Year's Seed Rain and Canopy Trees")
abline(lm(trees$sumseeds~trees$sumcan))
summary(lm(trees$sumseeds~trees$sumcan))
text(sumseeds~sumcan, labels=site, data= trees, cex=0.8, font=3)

## how well does nurse log sa line up with no germs?
plot(sumdata$sumg~sumdata$SA, pch=19, xlab="Sum of Conifer Seeds", 
     cex=3, log='x',
     ,ylab="Sum of Nurse Log SA", 
     col=ifelse(trees$urban==0, "#B8D685", "#FFCA85")
     , main="This Year's Germinantss and Nurse Log SA")
abline(lm(sumdata$sumg~sumdata$SA))
summary(lm(sumdata$sumg~sumdata$SA))
text(sumg~SA, labels=Forest, data= sumdata, cex=0.8, font=3)

#####################################################################
### transforming expanded plot data with germ-can
## gather function
tidySpp<- prelim %>% 
gather(key = "type", value="count", TSHEg:QUGAcan)

## separate age and sp
tidySpp<-tidySpp %>% extract(type, c("species", "age"), "([A-Z]+)([a-z]+)")
# remove things with 0's
tidySppNoZ<- tidySpp[(tidySpp$count>0),]
#age as a factor
tidySpp$age <- factor(tidySpp$age, levels=c("g", "s", "sm", "lg", "can"))
par(mfrow=c(2, 5))
plot(tidySppNoZ$count[tidySppNoZ$SiteName=="Riverview"]~tidySppNoZ$age[tidySppNoZ$SiteName=="Riverview"],
     xlab="Age", ylab="No", main="Riverview")
plot(tidySppNoZ$count[tidySppNoZ$SiteName=="Tryon"]~tidySppNoZ$age[tidySppNoZ$SiteName=="Tryon"],
     xlab="Age", ylab="No", main="Tryon")
plot(tidySppNoZ$count[tidySppNoZ$SiteName=="Marquam"]~tidySppNoZ$age[tidySppNoZ$SiteName=="Marquam"])
plot(tidySppNoZ$count[tidySppNoZ$SiteName=="Lacamas"]~tidySppNoZ$age[tidySppNoZ$SiteName=="Lacamas"])
plot(tidySppNoZ$count[tidySppNoZ$SiteName=="ForestPark"]~tidySppNoZ$age[tidySppNoZ$SiteName=="ForestPark"])
plot(tidySppNoZ$count[tidySppNoZ$SiteName=="Barlow"]~tidySppNoZ$age[tidySppNoZ$SiteName=="Barlow"])
plot(tidySppNoZ$count[tidySppNoZ$SiteName=="McIver"]~tidySppNoZ$age[tidySppNoZ$SiteName=="McIver"])
plot(tidySppNoZ$count[tidySppNoZ$SiteName=="Sandy"]~tidySppNoZ$age[tidySppNoZ$SiteName=="Sandy"])
plot(tidySppNoZ$count[tidySppNoZ$SiteName=="Oxbow"]~tidySppNoZ$age[tidySppNoZ$SiteName=="Oxbow"])
plot(tidySppNoZ$count[tidySppNoZ$SiteName=="Wildwood"]~tidySppNoZ$age[tidySppNoZ$SiteName=="Wildwood"])

#need a table withs mean and sd 
avg<-with(tidySpp, tapply(count, list("SiteName"=SiteName, "Age"=age),mean))
s<-with(tidySpp, tapply(count, list("SiteName"=SiteName, "AgeS"=age), sd))
plot(avg)


