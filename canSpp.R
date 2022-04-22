
# does the canopy spp vary

tidySpp <- read_csv("data/tidySpp.csv")

# get only the conifers
tidySppCon <- filter(tidySpp, morph == "con")

# combine conifers by species so its all conifers
conAgeBySp <- tidySppCon %>%
  group_by(age, Urban, SiteName, PlotID, species) %>%
  summarize(count = sum(count))

conAgeBySp$age <- factor(conAgeBySp$age , levels=c("g", "s", "sm", "lg", "can"))

# only canopy
canopySp <- filter(conAgeBySp, age =="can")
canopySp <- canopySp %>% group_by(Urban, SiteName, species) %>%
  summarize(count = sum(count))

summary(aov(log10(1+canopySp$count) ~ canopySp$species * canopySp$Urban))

canopySp$SiteName <- as.factor(canopySp$SiteName)
canopySp$SiteName <- factor(canopySp$SiteName , levels=c("ForestPark", "Lacamas", "Marquam", 
                                                      "Riverview","Tryon", "Barlow", 
                                                      "McIver", "Oxbow", "Sandy", "Wildwood"))

canopySp$Urban <- factor(canopySp$Urban, levels=c("urban", "rural"))

canopySp$species <- recode_factor(canopySp$species, AB="True firs", PSME="Douglas fir", 
                                  THPL = "Western redcedar", TSHE="Western hemlock")

ggplot(data = canopySp, aes(x = species, y = count, fill = Urban)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = c("#fdcdac", "#b3e2cd")) +
  theme_light() +
  theme(text=element_text(size=15)) +
  ggtitle("") +
  xlab("")+
  ylab(bquote('Canopy trees per sampled 30 m' ^ 2))


conSeedSummary <- read_csv("data/conSeedSummary.csv")

conSeedSite <- conSeedSummary %>% 
  group_by(Urban, SiteName) %>% 
  summarize(meanSeed=mean(a, na.rm=TRUE), seSeed=std.error(a, na.rm=TRUE))

poo <- canopySp %>% inner_join(conSeedSite,by="SiteName")

ggplot(data = poo, aes(x = count , y = meanSeed, color=species)) +
  geom_point() +
  geom_smooth(method="lm")

summary(aov(meanSeed ~ count, data = poo[poo$species=="TSHE",]))
summary(aov(meanSeed ~ count, data = poo[poo$species=="PSME",]))
summary(aov(meanSeed ~ count, data = poo[poo$species=="AB",]))
summary(aov(meanSeed ~ count, data = poo[poo$species=="THPL",]))
summary(aov(meanSeed ~ count * species, data = poo)) # this doesn't quite answer my question

poo %>%
  group_by(Urban.x, species) %>%
  summarise(mean(count), std.error(count))
