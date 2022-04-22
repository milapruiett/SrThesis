# Are there fewer young conifers?
library(multcomp)

survey<-read_csv("data/seedling.csv") 

urban <- c("Lacamas", "ForestPark", "Riverview", "Marquam", "Tryon")
rural <- c("McIver", "Oxbow", "Wildwood", "Sandy", "Barlow")

survey$SiteName <- factor(survey$SiteName , levels=c( "ForestPark", "Lacamas", "Marquam", 
                                                                  "Riverview","Tryon", "Barlow", 
                                                                  "McIver", "Oxbow", "Sandy", "Wildwood"))

survey <- survey %>% 
  mutate(Urban=case_when(SiteName %in% urban ~ 'urban', SiteName %in% rural ~ 'rural', TRUE ~ NA_character_))
survey$Urban <- as.factor(survey$Urban)

conG <- survey %>% select("SiteName", "Urban", "PlotID", "CONg")
write.csv(conG, "data/conG.csv")

as.factor(conG$Urban)

conG$Urban <- factor(conG$Urban, levels=c("urban", "rural"))

jpeg("output/coniferGerms.jpg")
ggplot(data = survey, aes(x = SiteName, y = CONg, fill = as.factor(Urban))) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = c("#b3e2cd","#fdcdac")) +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab(bquote('Conifer germinants in sampled 5 m' ^2)) + 
  xlab(" ") +
  theme(text=element_text(size=15)) +
  ggtitle("")
dev.off()

# how to analyze the seedling data, nested anova
anova <- aov(CONg ~ Urban / SiteName, data = survey)
summary(anova)

# Tukey HSD test:
mod1 <- aov(CONg ~ Urban + SiteName, data = survey)
tukeyfit1 <- TukeyHSD(mod1, conf.level=.95)
tukeyfit1



# get the mean by urban / rural
survey %>%
  group_by(Urban) %>%
  summarise(mean(CONg), std.error(CONg))

        