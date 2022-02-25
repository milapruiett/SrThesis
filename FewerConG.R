# Are there fewer young conifers?
survey<-read_csv("seedling.csv") 
survey$SiteName <- factor(survey$SiteName , levels=c("Barlow", 
                                                     "McIver", "Oxbow", "Sandy", "Wildwood", 
                                                     "ForestPark", "Lacamas", "Marquam", "Riverview", 
                                                     "Tryon"))
urban <- c("Lacamas", "ForestPark", "Riverview", "Marquam", "Tryon")
rural <- c("McIver", "Oxbow", "Wildwood", "Sandy", "Barlow")

survey <- survey %>% 
  mutate(Urban=case_when(SiteName %in% urban ~ 'urban', SiteName %in% rural ~ 'rural', TRUE ~ NA_character_))
survey$Urban <- as.factor(survey$Urban)


ggplot(data = prelim, aes(x = SiteName, y = CONg, fill = as.factor(Urban))) +
  geom_boxplot() +
  scale_fill_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab(bquote('Conifer Gemrinatns in 0.5 m'^2)) + 
  xlab(" ") +
  ggtitle("Rural forests have more conifer germinants than urban forests")
  
# how to analyze the seedling data, nested anova
summary(aov(CONg ~ Urban / SiteName, data = survey))

        