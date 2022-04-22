install.packages("vegan")
library("vegan")
install.packages("ggplot2")
library("ggplot2")

survey<-read.csv("data/seedling.csv")
urban <- c("Lacamas", "ForestPark", "Riverview", "Marquam", "Tryon")
rural <- c("McIver", "Oxbow", "Wildwood", "Sandy", "Barlow")

survey <- survey %>% 
  mutate(Urban=case_when(SiteName %in% urban ~ 'urban', SiteName %in% rural ~ 'rural', TRUE ~ NA_character_))

# one acci entry is incorrect, fix it
survey$ACCIcan <- c(0)
canDF <- survey[, c(1:3, 41:44)]


canopyCon <- tidySpp[tidySpp$age=="can" & tidySpp$morph=="con" , ]

CAN <- canopyCon %>%
  group_by(SiteName, Urban, species) %>%
  summarise("sum" = sum(count)) 

CAN <- CAN %>% spread(species, sum)


adonis(CAN[, 3:6] ~ CAN$Urban)


 # create a df with only canopy conifers
canDF <- survey[rowSums(survey[41:44])>0 , c(1:3, 41:44)]
canDF <- canDF[, colSums(canDF != 0) > 0]

canDFmodel=metaMDS(canDF[ ,c(4:7)], trymax=500)

plot(canDFmodel, display="sites") 

# color over according to whether urban or rural
points(canDFmodel$points[canDF[,2]=="urban",], pch=16, col="brown")
points(canDFmodel$points[canDF[,2]=="rural",], pch=16, col="darkgreen")

# can also add circles representing 95% confidence intervals
ordiellipse(canDFmodel, groups=canDF[,2], kind="se", conf=0.95)  # can do various things here to make circles of different colors or add other info

ordiellipse(canDFmodel, groups=canDF[,2]=="urban", kind="se", conf=0.95, show.groups=T, lwd=2, col="brown")  # can do various things here to make circles of different colors or add other info
ordiellipse(canDFmodel, groups=canDF[,2]=="rural", kind="se", conf=0.95, show.groups=T, lwd=2, col="darkgreen")  # can do various things here to make circles of different colors or add other info

# Can add arrows that show direction correlated with greatest change in abundance of certain species:
treefit = envfit(canDFmodel, canDF[,c(4:7)])
plot(treefit)
