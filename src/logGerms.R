# do nurse logs line up with germinants?

conG <- read_csv("data/conG.csv")
completeWood <- read_csv("data/completeWood45.csv")

# multiply so that wood is in cm per sq m
completeWood <- completeWood %>%
  mutate(a = a/ 1500)

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

jpeg("output/woodGermSmooth.jpg")
ggplot(data = woodGerm, aes(x=meanW, y=meanG)) + 
  geom_smooth(method=lm) +
  geom_point(size = 4, aes(color = Urban)) +
  geom_text(aes(label = SiteName), colour="black", size = 3)+
  scale_color_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab("Mean no. conifer germinants") + 
  xlab("Mean surface area of logs decay classes 4 and 5, sq cm per sq m sampled") +
  ggtitle("Conifer germinants and logs")
dev.off()

# with cross erros
jpeg("output/woodGermCrossErros.jpg")
ggplot(data = woodGerm, aes(x = meanW, y = meanG)) +
  geom_errorbar(aes(ymin = meanG - seG, ymax = meanG + seG), col="grey") + 
  geom_errorbar(aes(xmin = meanW - seW, xmax = meanW + seW), col="grey") +
  scale_color_brewer(palette="Pastel2") +
  geom_point(size = 4, aes(color = Urban)) +
  geom_text(aes(label = SiteName), colour="black", size = 3)+
  theme_light() +
  ylab("No germinants per forest") +
  xlab("Surface area decay classes 4 and 5 logs, sq cm per sq m sampled") +
  ggtitle("Conifer germinants and logs")
dev.off()
