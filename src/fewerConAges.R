# are there fewer conifers of all ages?
library("pscl")
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

# what is the distribution to pick the correct stat test
ggplot(data = conAgeByPlot, aes(x = count)) +
  facet_wrap(~ age) + 
  geom_histogram()

# what percent of the data are zeros
100*sum(conAgeByPlot == 0)/nrow(conAgeByPlot) # = 75% of the plots are 0s

mod1 <- zeroinfl(formula = count ~ Urban | Urban, dist = 'poisson', data = seedlings)
summary(mod1)


# so everything is per 30 m^2
young <- c("g", "s")
old <- c("sm", "lg", "can")
conAgeByPlot <- conAgeByPlot %>% 
  mutate(count=case_when(age %in% young ~ count*6, age %in% old ~ count*1))

# fewer germs?
germ <- filter(conAgeByPlot, age =="g")
wilcox.test(count ~ Urban, germ)
t.test(log10(1 +count) ~ Urban, germ) 
mod1 <- aov(log10(1 + germ$count) ~ germ$SiteName)
tukeyfit1 <- TukeyHSD(mod1, conf.level=.95)
tukeyfit1

# fewer seedlings
seedlings <- filter(conAgeByPlot, age =="s")
wilcox.test(count ~ Urban, seedlings)  
t.test(log10(1 +count) ~ Urban, seedlings) 
mod1 <- aov(log10(1 + seedlings$count) ~ seedlings$SiteName)
tukeyfit1 <- TukeyHSD(mod1, conf.level=.95)
tukeyfit1
summary(m1 <- glm(count ~ Urban, family="poisson", data=can))

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
  geom_boxplot() +
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
  theme(text=element_text(size=15)) +
  ggtitle("") +
  ylab(bquote('No. conifers per 30 m' ^ 2))
dev.off()

# get summaries of means
conAgeByPlot %>%
  group_by(age, Urban) %>%
  summarise(mean(count), std.error(count))

summary(aov(tidySppCon$count ~ tidySppCon$species * tidySppCon$Urban * tidySppCon$age))
tidyConCan <- filter(tidySppCon, age == "can")
summary(aov(tidyConCan$count ~ tidyConCan$species * tidyConCan$Urban))


# does the species of conifer vary between urban and rural?
ggplot(tidySppCon, aes(fill=species, y=count, x=Urban)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~ age, scale="free") +
  scale_fill_brewer(palette="Set3") 


