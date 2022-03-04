# do nurse logs line up with germinants?

conG <- read_csv("conG.csv")
completeWood <- read_csv("completeWood.csv")

# summarize

conGermSummary <- conG %>% 
  group_by(Urban, SiteName) %>% 
  summarize(meanG=mean(CONg, na.rm=TRUE), seG = std.error(CONg, na.rm=TRUE))

woodSummary <- completeWood %>% 
  group_by(Forest) %>% 
  summarize(meanW=mean(a, na.rm=TRUE), seW = std.error(a, na.rm=TRUE))

# rename so all forests have same names
woodSummary$Forest <- c("Barlow", "McIver", "Oxbow", "Sandy", 
                           "Wildwood", "ForestPark", "Lacamas", 
                           "Marquam", "Riverview", "Tryon")

# inner_join()
woodGerm= conGermSummary %>% inner_join(woodSummary,by=c("SiteName" ="Forest"))

#linear model
summary(lm(woodGerm$meanG ~ woodGerm$meanW))

# plot

# as a line

jpeg("woodGermSmooth.jpg")
ggplot(data = woodGerm, aes(x=meanW, y=meanG, label= SiteName)) + 
  geom_point(aes(color = Urban)) +
  geom_smooth(method=lm) +
  scale_color_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab("Mean no. conifer germinants") + 
  xlab("Mean surface area of logs decay classes 4 and 5") +
  ggtitle("Conifer germinants and logs")
dev.off()

# with cross erros
ggplot(data = woodGerm, aes(x = meanW, y = meanG, color = Urban, label =SiteName)) +
  geom_point() +
  geom_text(hjust=0, vjust=0) +
  geom_errorbar(aes(ymin = meanG - seG, ymax = meanG + seG), col="grey") + 
  geom_errorbar(aes(xmin = meanW - seW, xmax = meanW + seW), col="grey") +
  scale_color_brewer(palette="Pastel2") +
  theme_light() +
  ylab("No germinants per forest") +
  xlab("Surface area decay classes 4 and 5 logs") +
  ggtitle("Conifer germinants and logs")
