# are there fewer conifers of all ages?

tidySpp <- read_csv("data/tidySpp.csv")

# get only the conifers
tidySppCon <- filter(tidySpp, morph == "con")

# combine conifers by species so its all conifers
conAgeByPlot <- tidySppCon %>%
  group_by(age, Urban, SiteName, PlotID) %>%
  summarize(count = sum(count))

# qa/qc
perSite <- conAgeByPlot%>%
  group_by(SiteName, age) %>%
  summarize(quanitty= n())

summary(aov(count ~ age * Urban, data = conAgeByPlot))

# so everything is per 30 m^2
young <- c("g", "s")
old <- c("sm", "lg", "can")
conAgeByPlot <- conAgeByPlot %>% 
  mutate(count=case_when(age %in% young ~ count*6, age %in% old ~ count*1))

# fewer germs?
germ <- filter(conAgeByPlot, age =="g")
wilcox.test(count ~ Urban, germ)
t.test(log10(1 +count) ~ Urban, germ) 

# fewer seedlings
seedlings <- filter(conAgeByPlot, age =="s")
wilcox.test(count ~ Urban, seedlings)  
t.test(log10(1 +count) ~ Urban, seedlings) 

# fewer small
smsub<- filter(conAgeByPlot, age =="sm")
wilcox.test(count ~ Urban, smsub)   
t.test(log10(1 +count) ~ Urban, smsub) 

# fewer lg
lgsub <- filter(conAgeByPlot, age =="lg")
wilcox.test(count ~ Urban, lgsub)   
t.test(log10(1 +count) ~ Urban, lgsub) 

# fewer canopy
canopy <- filter(conAgeByPlot, age =="can")
wilcox.test(count ~ Urban, canopy)  
t.test(log10(1 +count) ~ Urban, can) 

# graph it
conAgeByPlot$age <- factor(conAgeByPlot$age , levels=c("g", "s", "sm", "lg", "can"))
conAgeByPlot$SiteName <- factor(conAgeByPlot$SiteName , levels=c("Barlow", 
                                                     "McIver", "Oxbow", "Sandy", "Wildwood", 
                                                     "ForestPark", "Lacamas", "Marquam", "Riverview", 
                                                                                                         "Tryon"))
jpeg("output/conAllAgesBySite.jpg", width=1400, height=680)
ggplot(data = conAgeByPlot, aes(x = SiteName, y = count, fill = Urban)) +
  facet_wrap(~ age) +
  geom_violin() +
  scale_y_continuous(trans='pseudo_log') +
  scale_fill_brewer(palette="Pastel2") +
  theme_light() 
dev.off()

jpeg("output/conAllAgesByUrban.jpg", width=1400, height=680)
ggplot(data = conAgeByPlot, aes(x = age, y = count, fill = Urban)) +
  geom_boxplot(outlier.shape = 21) +
  scale_y_continuous(trans='pseudo_log') +
  scale_fill_brewer(palette="Pastel2") +
  theme_light() +
  ggtitle("")+
  ylab(bquote('Conifer Number in sampled 30 m' ^2)) 
dev.off()

# get summaries of means
conAgeByPlot %>%
  group_by(age, Urban) %>%
  summarise(mean(count))




