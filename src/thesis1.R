prelim<-read.csv("seedling.csv") 
foo<- read.csv("skippingR.csv") 
install.packages("tidyverse")
library("tidyverse")
#i think i want the 0s to say rural and the 1's to say urban
foo$urban[foo$urban==1] <- "Urban"
foo$urban[foo$urban==0] <- "Rural"

#how to count things from seedling dataset to create foo
tapply(prelim$CONg , prelim$SiteName, sum)
plot(prelim$CONg~prelim$SiteName)

#ensure urban is a factor in foo
foo$urban<-as.factor(foo$urban)


#how to look at different age groups across sites
prelim$SiteName <- factor(prelim$SiteName , levels=c("Barlow", "McIver", "Oxbow", "Sandy", "Wildwood", "ForestPark", "Lacamas", "Marquam", "Riverview", "Tryon"))
germplot=plot(prelim$CONg~prelim$SiteName, xlab = "Forest Name", ylab="Average No. Conifer Germinants in 0.5 sq cm", main = "Rural forests have more conifer germinants than urban forests",
     col=c("mistyrose",
           "mistyrose", "mistyrose", "mistyrose", "mistyrose", 
           "powderblue", "powderblue","powderblue", "powderblue", "powderblue"))
l=legend(x= "topright", legend=c("Rural", "Urban"), fill=c("mistyrose", "powderblue"))

# how to analyze the seedling data
summary(aov(prelim$CONg~ prelim$Urban))



#wilcox test
wilcox.test(foo$sumsm~foo$urban)

#regular r graphing of foo
par(mfrow=c(2, 3))
boxplot(foo$sumg ~foo$urban, main="Germinants", col=c("palegreen3", "grey"))
boxplot(foo$sums ~foo$urban, main="Seedlings", col=c("palegreen3", "grey"))
boxplot(foo$sumsm ~foo$urban, main="Small",col=c("palegreen3", "grey"))
boxplot(foo$sumlg ~foo$urban, main="Large", col=c("palegreen3", "grey"))
boxplot(foo$sumcan ~foo$urban, main="Canopy", col=c("palegreen3", "grey"))

#just seeds and canopy trees
boxplot(foo$sumseeds ~foo$urban, main="Rural forests have more conifer seed rain than urban forests",
        ylab="Total number of conifer seeds in one forest", 
        xlab="Wilcoxon test, p=0.05",
        col=c("mistyrose", "powderblue"))


wilcox.test(foo$sumseeds~foo$urban)

#try to do it in ggplot
p<-ggplot(foo, aes(x=urban, y=sumg, color=urban)) +geom_boxplot()
p #couldnt quite figure out colors




#group by urban vs rural
table=tapply(prelim$CONg  , prelim$SiteName, mean)

#how to look at age structure within a site

# is there a way to see what species are represented at each age?
#nmds?
#look at data for just canopy
small<-prelim[1:149,41:49]
#i seem to have missing values
sum(is.na(small))
view(small)
# i think i need to remove all rows that have 0s for everything
small <-small[rowSums(small[])>0,]

#dist matrix
island.spp_distmat <- 
  vegdist(island.spp.rel, method = "bray")
island.spp_distmat <- 
  as.matrix(island.spp_distmat, labels = T)
write.csv(island.spp_distmat, "island_spp_distmat.csv")

small_distmat <- vegdist(small, method="bray")
small_NMS <-
  metaMDS(small, distance ="bray",
          k=1, maxit =999,
          trymax=500,autotransform = FALSE,
          wascores=TRUE)




