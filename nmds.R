install.packages("vegan")
library("vegan")
install.packages("ggplot2")
library("ggplot2")

survey<-read.csv("seedling.csv")
urban <- c("Lacamas", "ForestPark", "Riverview", "Marquam", "Tryon")
rural <- c("McIver", "Oxbow", "Wildwood", "Sandy", "Barlow")

survey <- survey %>% 
  mutate(Urban=case_when(SiteName %in% urban ~ 'urban', SiteName %in% rural ~ 'rural', TRUE ~ NA_character_))

# one acci entry is incorrect, fix it
survey$ACCIcan <- c(0)


# germinants
# remove plots where there is nothing and columns with nothing
surveyNoZeros <- survey[rowSums(survey[4:12])>0 , 1:12]
surveyNoZeros <-surveyNoZeros[, colSums(surveyNoZeros != 0) > 0]

myNMDSmodel=metaMDS(surveyNoZeros[,4:11], trymax=500)

plot(myNMDSmodel, display="sites") 

# color over according to whether urban or rural
points(myNMDSmodel$points[surveyNoZeros[,2]=="urban",], pch=16, col="brown")
points(myNMDSmodel$points[surveyNoZeros[,2]=="rural",], pch=16, col="darkgreen")

# can also add circles representing 95% confidence intervals
ordiellipse(myNMDSmodel, groups=surveyNoZeros[,2], kind="se", conf=0.95)  # can do various things here to make circles of different colors or add other info

ordiellipse(myNMDSmodel, groups=surveyNoZeros[,2]=="urban", kind="se", conf=0.95, show.groups=T, lwd=2, col="brown")  # can do various things here to make circles of different colors or add other info
ordiellipse(myNMDSmodel, groups=surveyNoZeros[,2]=="rural", kind="se", conf=0.95, show.groups=T, lwd=2, col="darkgreen")  # can do various things here to make circles of different colors or add other info


# Can add arrows that show direction correlated with greatest change in abundance of certain species:
treefit = envfit(myNMDSmodel, surveyNoZeros[,4:11])
plot(treefit)

# seedlings
surveyNoZeros <- survey[rowSums(survey[13:22])>0 , 1:22]
surveyNoZeros <-surveyNoZeros[, colSums(surveyNoZeros != 0) > 0]

myNMDSmodel=metaMDS(surveyNoZeros[ ,11:19], trymax=500)

plot(myNMDSmodel, display="sites") 

# color over according to whether urban or rural
points(myNMDSmodel$points[surveyNoZeros[,2]=="urban",], pch=16, col="brown")
points(myNMDSmodel$points[surveyNoZeros[,2]=="rural",], pch=16, col="darkgreen")

# can also add circles representing 95% confidence intervals
ordiellipse(myNMDSmodel, groups=surveyNoZeros[,2], kind="se", conf=0.95)  # can do various things here to make circles of different colors or add other info

ordiellipse(myNMDSmodel, groups=surveyNoZeros[,2]=="urban", kind="se", conf=0.95, show.groups=T, lwd=2, col="brown")  # can do various things here to make circles of different colors or add other info
ordiellipse(myNMDSmodel, groups=surveyNoZeros[,2]=="rural", kind="se", conf=0.95, show.groups=T, lwd=2, col="darkgreen")  # can do various things here to make circles of different colors or add other info


# Can add arrows that show direction correlated with greatest change in abundance of certain species:
treefit = envfit(myNMDSmodel, surveyNoZeros[,11:19])
plot(treefit)

### small subcanopy

surveyNoZeros <- survey[rowSums(survey[23:31])>0 , 1:31]
surveyNoZeros <-surveyNoZeros[, colSums(surveyNoZeros != 0) > 0]

myNMDSmodel=metaMDS(surveyNoZeros[ ,19:26], trymax=500)

plot(myNMDSmodel, display="sites") 

# color over according to whether urban or rural
points(myNMDSmodel$points[surveyNoZeros[,2]=="urban",], pch=16, col="brown")
points(myNMDSmodel$points[surveyNoZeros[,2]=="rural",], pch=16, col="darkgreen")

# can also add circles representing 95% confidence intervals
ordiellipse(myNMDSmodel, groups=surveyNoZeros[,2], kind="se", conf=0.95)  # can do various things here to make circles of different colors or add other info

ordiellipse(myNMDSmodel, groups=surveyNoZeros[,2]=="urban", kind="se", conf=0.95, show.groups=T, lwd=2, col="brown")  # can do various things here to make circles of different colors or add other info
ordiellipse(myNMDSmodel, groups=surveyNoZeros[,2]=="rural", kind="se", conf=0.95, show.groups=T, lwd=2, col="darkgreen")  # can do various things here to make circles of different colors or add other info

# Can add arrows that show direction correlated with greatest change in abundance of certain species:
treefit = envfit(myNMDSmodel, surveyNoZeros[,19:26])
plot(treefit)



### large subcanopy
surveyNoZeros <- survey[rowSums(survey[32:40])>0 , 1:40]
surveyNoZeros <-surveyNoZeros[, colSums(surveyNoZeros != 0) > 0]

myNMDSmodel=metaMDS(surveyNoZeros[ ,22:28], trymax=500)

plot(myNMDSmodel, display="sites") 

# color over according to whether urban or rural
points(myNMDSmodel$points[surveyNoZeros[,2]=="urban",], pch=16, col="brown")
points(myNMDSmodel$points[surveyNoZeros[,2]=="rural",], pch=16, col="darkgreen")

# can also add circles representing 95% confidence intervals
ordiellipse(myNMDSmodel, groups=surveyNoZeros[,2], kind="se", conf=0.95)  # can do various things here to make circles of different colors or add other info

ordiellipse(myNMDSmodel, groups=surveyNoZeros[,2]=="urban", kind="se", conf=0.95, show.groups=T, lwd=2, col="brown")  # can do various things here to make circles of different colors or add other info
ordiellipse(myNMDSmodel, groups=surveyNoZeros[,2]=="rural", kind="se", conf=0.95, show.groups=T, lwd=2, col="darkgreen")  # can do various things here to make circles of different colors or add other info

# Can add arrows that show direction correlated with greatest change in abundance of certain species:
treefit = envfit(myNMDSmodel, surveyNoZeros[,22:28])
plot(treefit)


### canopy
CansurveyNoZeros <- survey[rowSums(survey[41:49])>0 , c(1:3, 41:49)]
CansurveyNoZeros <-CansurveyNoZeros[, colSums(CansurveyNoZeros != 0) > 0]

CanmyNMDSmodel=metaMDS(CansurveyNoZeros[ ,c(4:10)], trymax=500)

plot(CanmyNMDSmodel, display="sites") 

# color over according to whether urban or rural
points(CanmyNMDSmodel$points[CansurveyNoZeros[,2]=="urban",], pch=16, col="brown")
points(CanmyNMDSmodel$points[CansurveyNoZeros[,2]=="rural",], pch=16, col="darkgreen")

# can also add circles representing 95% confidence intervals
ordiellipse(myNMDSmodel, groups=surveyNoZeros[,2], kind="se", conf=0.95)  # can do various things here to make circles of different colors or add other info

ordiellipse(myNMDSmodel, groups=surveyNoZeros[,2]=="urban", kind="se", conf=0.95, show.groups=T, lwd=2, col="brown")  # can do various things here to make circles of different colors or add other info
ordiellipse(myNMDSmodel, groups=surveyNoZeros[,2]=="rural", kind="se", conf=0.95, show.groups=T, lwd=2, col="darkgreen")  # can do various things here to make circles of different colors or add other info

# Can add arrows that show direction correlated with greatest change in abundance of certain species:
treefit = envfit(myNMDSmodel, surveyNoZeros[,c(32:38)])
plot(treefit)



