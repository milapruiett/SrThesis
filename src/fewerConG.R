# Are there fewer young conifers?
survey<-read_csv("data/seedling.csv") 
survey$SiteName <- factor(survey$SiteName , levels=c("Barlow", 
                                                     "McIver", "Oxbow", "Sandy", "Wildwood", 
                                                     "ForestPark", "Lacamas", "Marquam", "Riverview", 
                                                     "Tryon"))

urban <- c("Lacamas", "ForestPark", "Riverview", "Marquam", "Tryon")
rural <- c("McIver", "Oxbow", "Wildwood", "Sandy", "Barlow")

survey <- survey %>% 
  mutate(Urban=case_when(SiteName %in% urban ~ 'urban', SiteName %in% rural ~ 'rural', TRUE ~ NA_character_))
survey$Urban <- as.factor(survey$Urban)

conG <- survey %>% select("SiteName", "Urban", "PlotID", "CONg")
write.csv(conG, "data/conG.csv")

jpeg("output/coniferGerms.jpg")
ggplot(data = prelim, aes(x = SiteName, y = CONg, fill = as.factor(Urban))) +
  geom_boxplot() +
  scale_fill_brewer(palette="Pastel2", name = "Urban", labels = c("Rural", "Urban")) +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab(bquote('Conifer Germinants in sampled 0.5 m' ^2)) + 
  xlab(" ") +
  ggtitle("Rural forests have more conifer germinants than urban forests")
dev.off()

# how to analyze the seedling data, nested anova
summary(aov(CONg ~ Urban / SiteName, data = survey))

        