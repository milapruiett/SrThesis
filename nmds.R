install.packages("vegan")
library("vegan")
install.packages("ggplot2")
library("ggplot2")


### germ nmds
prelim<-read.csv("seedling.csv")
small<-prelim[1:149,1:12] #here is where you select which age 41:49 is can

#i seem to have missing values
#sum(is.na(small))
#zeros<-small[rowSums(small[])==0,] #75 rows have all 0's for germs
view(small)
# i think i need to remove all rows that have 0s for everything
small <-small[rowSums(small[4:12])>0,]
# remove spp that don't occur
apply(small, 2, sum) #shows where the spp didn't occur anywhere
small[,(apply(small, 2, sum))>0] # removing that spp
#smallurban<- small[,1:3]
#small <-small[,4:12]

#prep the data dist matrix (this is the relative abundance stuff, not sure if necessary)
small_rel <- decostand(small, method="total") #relative abundance
small_distmat <- vegdist(small_rel, method="bray") #make the dist matrix with bray
view(small_distmat)
small_distmat <- 
  as.matrix(small_distmat, labels = T)
write.csv(small_distmat, "small_dismat.csv") #create a file

#run the nms
small_NMS <-
  metaMDS(small[,4:12], 
          k=2, maxit =999,
          trymax=500,autotransform = FALSE,
          wascores=TRUE)

#paulette's r
plot(small_NMS,display="species",type="n",family="serif",xlim=c(-1.5,1.5),ylim=c(-1.5,1.5)) #This  creates a blank NMDS graph
points(small_NMS,select=(small$Urban==1),pch=16,col="forestgreen",cex=1.25) #This adds points to that NMDS graph, separating groups of data points by treatment. The function pch specifies the type of symbol used for each treatment.  Here we use two kinds of circles, some filled (pch=16) and some open (pch=1). The cex function allows us to change the size of the datapoints. This example code uses red for block 2 and green for block 1. There are 4 lines of code, each specifying a different tmt and block combination. If block is not specified, then only 2 lines are needed.
points(small_NMS,select=(small$Urban==0),pch=16,col="grey",cex=1.25) 

axes for each species, the goodness of fit statistic (r-squared), and the p-value. Expect many/most species to have sigt p-values because this is the point of the ordination. 
plot(spvectors)   #will plot all the species as vectors on the ordination plot. The longer the vector, the higher the r2 value. (is vector length equivalent to r?)

# Shepards test/goodness of fit
goodness(small_NMS) # Produces a results of test statistics for goodness of fit for each point
stressplot(small_NMS) # Produces a Shepards diagram
# i have no idea how to interpret this
datascores <- as.data.frame(scores(small_NMS))  #extract the site scores

#Add/calculate spider diagram
scores <- cbind(as.data.frame(datascores), Urban = smallurban$Urban)
centroids <- aggregate(cbind(NMDS1, NMDS2) ~ Urban, data = scores, FUN = mean)
seg <- merge(scores, setNames(centroids, c('Urban','oNMDS1','oNMDS2')),
             by = 'Urban', sort = FALSE)

#plot
ggplot(scores, aes(x = NMDS1, y = NMDS2, colour = Urban)) +
  geom_segment(data = seg,
               mapping = aes(xend = oNMDS1, yend = oNMDS2)) + # add spiders
  geom_point(data = centroids, size = 4) +                    # add centroids
  geom_point() +                                              
  coord_fixed()+                                              
  theme_bw()+ 
  theme(legend.position="right",legend.text=element_text(size=10),legend.direction='vertical')

# canopy nmds
big<-prelim[1:149,c(1:3, 32:40)] #here is where you select which age 41:49 is can
# i think i need to remove all rows that have 0s for everything
big <-big[rowSums(big[4:12])>0,]
bigurban<- big[,1:3]
big <-big[,4:12]

#prep the data dist matrix
big_rel <- decostand(big, method="total") #relative abundance
big_distmat <- vegdist(big_rel, method="bray") #make the dist matrix with bray
view(big_distmat)
big_distmat <- 
  as.matrix(big_distmat, labels = T)
write.csv(big_distmat, "big_dismat.csv") #create a file

#run the nms
big_NMS <-
  metaMDS(big_distmat, distance ="bray",
          k=2, maxit =999,
          trymax=500,autotransform = FALSE,
          wascores=TRUE)

# Shepards test/goodness of fit
goodness(big_NMS) # Produces a results of test statistics for goodness of fit for each point
stressplot(big_NMS) # Produces a Shepards diagram
# i have no idea how to interpret this
datascoresbig <- as.data.frame(scores(big_NMS))  #extract the site scores

#Add/calculate spider diagram
scores2 <- cbind(as.data.frame(datascoresbig), Urban = bigurban$Urban)
centroids2 <- aggregate(cbind(NMDS1, NMDS2) ~ Urban, data = scores2, FUN = mean)
seg2 <- merge(scores2, setNames(centroids2, c('Urban','oNMDS1','oNMDS2')),
             by = 'Urban', sort = FALSE)

#plot
ggplot(scores2, aes(x = NMDS1, y = NMDS2, colour = Urban)) +
  geom_segment(data = seg2,
               mapping = aes(xend = oNMDS1, yend = oNMDS2)) + # add spiders
  geom_point(data = centroids2, size = 4) +                    # add centroids
  geom_point() +                                              
  coord_fixed()+                                              
  theme_bw()+ 
  theme(legend.position="right",legend.text=element_text(size=10),legend.direction='vertical')




#nms plot
#plot(small_NMS, "sites")   # Produces distance 
#orditorp(small_NMS, "sites")  
# add colors to plot
#grp <- factor(rep(c('urban', 'rural'), each = 75))
#cols <- c('grey','green')
#text(small_NMS, display = "sites", col = rep(cols, each = 75))

## varespec has on 24 observations so 1-12 will be grassland and 13-24 quarry
#grp <- factor(rep(c('urban', 'rural'), each = 74))

## vector of colours
#cols <- c('red', 'black')

#set.seed(1)
#ord <- metaMDS(small_distmat, trace=FALSE)
#plot(ord, type = 'n')
#text(ord, display = 'sites', col = cols[grp])
#legend('bottomright', legend == tools::toTitleCase(levels(grp)), fill = cols, bty = 'n')

#ordiplot(example_NMDS,type="n")
#orditorp(small_NMS,display="species",col="red",air=0.01)
#orditorp(small_NMS,display="sites",cex=1.25,air=0.01)