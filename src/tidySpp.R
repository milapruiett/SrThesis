# canopy and germ, with error bars
install.packages("tidyverse")
library("plotrix", "psych", "tidyverse", "ggplot2")
prelim<-read.csv("seedling.csv") 
prelim$SiteName <- factor(prelim$SiteName , levels=c("Barlow", 
                                                     "McIver", "Oxbow", "Sandy", "Wildwood", 
                                                     "ForestPark", "Lacamas", "Marquam", "Riverview", 
                                                     "Tryon"))
tidySpp<- prelim %>% 
  gather(key = "type", value="count", TSHEg:QUGAcan)

## separate age and sp
tidySpp<-tidySpp %>% extract(type, c("species", "age"), "([A-Z]+)([a-z]+)")
tidyCon<-tidySpp[tidySpp$species=="CON",]

# remove things with 0's also cons
#tidySppNoZ<- tidyCon[(tidyCon$count>0),]
tidySppNoZ<- tidyCon
#age as a factor
tidySppNoZ$age <- factor(tidySppNoZ$age, levels=c("g", "s", "sm", "lg", "can"))

young <- c("g", "s")
old <- c("sm", "lg", "can")
tidySpp <- tidySpp %>% 
  mutate(count=case_when(age %in% young ~ count*6, age %in% old ~ count*1))

conifers <- c("TSHE", "THPL", "ABsp", "PSME")
decid <- c("ALRU", "ACCI", "ALVI", "ACMA")
urban <- c("Lacamas", "ForestPark", "Riverview", "Marquam", "Tryon")
rural <- c("McIver", "Oxbow", "Wildwood", "Sandy", "Barlow")

tidySpp <- tidySpp %>% 
  mutate(Urban=case_when(SiteName %in% urban ~ 'urban', SiteName %in% rural ~ 'rural', TRUE ~ NA_character_))

tidySpp <- tidySpp %>% 
  mutate(morph=case_when(species %in% conifers ~ 'con', species %in% decid ~ 'dec', TRUE ~ NA_character_))

tidySpp <- na.omit(tidySpp) 

 tidyConSummary <- tidySpp%>% 
  filter(morph == "con", na.rm = TRUE) %>% 
  group_by(age, Urban, SiteName) %>% 
  summarize(avg=mean(count), se=std.error(count))
 
 tidySpp <- tidySpp%>% 
   filter(morph == "con", na.rm = TRUE)
 
# look at germs in a different way
 tidyConSummary <- tidySpp%>% 
   filter(morph == "con", age == "g", na.rm = TRUE) %>% 
   group_by(age, Urban, SiteName) %>% 
   summarize(sum=sum(count))

 ggplot(data = tidyConSummary, aes(x = sum, fill = Urban)) + 
   geom_histogram() + 
   ylab(bquote('Sum of Conifer germinants in .5 '(m^2))) +
   scale_fill_brewer(palette="Pastel2") +
   theme_light() +
   ggtitle("The data is not normally distributed")
 
 ggplot(data = tidyConSummary, aes(y= sum, fill = Urban)) + 
   geom_boxplot() + 
   ylab(bquote('Sum of Conifer germinants in .5 '(m^2))) +
   scale_fill_brewer(palette="Pastel2") +
   theme_light() +
   ggtitle("Rural forests seem to have more germinants")
 




#create the table for se and avg
avg<-with(tidySpp, tapply(count, list("SiteName"=SiteName, "Age"=age), mean))
colnames(avg)<-c("canavg", "germavg", "lgavg", "seedavg", "smavg")
avg <- avg[, c("germavg", "seedavg", "smavg", "lgavg", "canavg")]
#the area I surveyed for can and germ was different, standardize to trees / 30 m sq
avg[,1:2]<-avg[,1:2]*6
s<-with(tidySpp, tapply(count, list("SiteName"=SiteName, "AgeS"=age), std.error))
colnames(s)<-c("canse", "germse", "lgse", "seedse", "smse")
s[,c(2,4)]<-s[,c(2,4)]*6
canGermTable<-data.frame(cbind(avg,s))
canGermTable$Urban <- as.factor(c("rural", "rural", "rural", "rural", "rural", "urban", "urban", "urban", "urban", "urban"))

write_csv(canGermTable, "canGermTable.csv")

#make graph
ggplot(data = canGermTable, aes(x = canavg, y = germavg, color=Urban)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = germavg-germse, ymax = germavg+germse), col="grey") + 
  geom_errorbar(aes(xmin = canavg-canse, xmax = canavg+canse), col="grey") +
  ylab(bquote('Conifer germinants in 30 '(m^2))) +
  xlab(bquote('Conifer canopy Trees in 30' (m^2))) +
  scale_color_brewer(palette="Pastel2") +
  theme_light() +
  ggtitle("More canopy trees = more germinants")


#cross error bar plot
ggplot(data = canGermTable, aes(x = canavg, y = germavg, color=Urban)) + geom_point() + 
  geom_smooth(aes(colour=NA),method=lm, se=F, fullrange=T) +
  ylab(bquote('Conifer germinants in 30 '(m^2))) +
  xlab(bquote('Conifer canopy Trees in 30' (m^2))) +
  scale_fill_brewer(palette="Pastel2") +
  theme_light() +
  ggtitle("More canopy trees = more germinants")


#test the line of best fit
summary(lm(canGermTable$germavg ~ canGermTable$canavg))

