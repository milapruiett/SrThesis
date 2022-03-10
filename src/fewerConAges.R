# are there fewer conifers of all ages?

tidySpp <- read_csv("data/tidySpp.csv")
tidySpp <- filter(tidySpp, morph == "con")
summary(aov(count ~ age * Urban, data = tidySpp))

# so everything is per 30 m^2
young <- c("g", "s")
old <- c("sm", "lg", "can")
tidySpp <- tidySpp %>% 
  mutate(count=case_when(age %in% young ~ count*6, age %in% old ~ count*1))

# fewer germs?
germ <- filter(tidySpp, age =="g")
wilcox.test(count ~ Urban, germ)

# fewer seedlings
seedlings <- filter(tidySpp, age =="s")
wilcox.test(count ~ Urban, seedlings)  
t.test(log10(1 +count) ~ Urban, seedlings) 

# fewer small
smsub<- filter(tidySpp, age =="sm")
wilcox.test(count ~ Urban, smsub)      

# fewer lg
lgsub <- filter(tidySpp, age =="lg")
wilcox.test(count ~ Urban, lgsub)   

# fewer canopy
canopy <- filter(tidySpp, age =="can")
wilcox.test(count ~ Urban, canopy)  

# graph it
tidySpp$age <- factor(tidySpp$age , levels=c("g", "s", "sm", "lg", "can"))
tidySpp$SiteName <- factor(tidySpp$SiteName , levels=c("Barlow", 
                                                     "McIver", "Oxbow", "Sandy", "Wildwood", 
                                                     "ForestPark", "Lacamas", "Marquam", "Riverview", 
             
                                                                                             "Tryon"))
jpeg("output/conAllAgesBySite.jpg", width=1400, height=680)
ggplot(data = tidySpp, aes(x = SiteName, y = count, fill = Urban)) +
  facet_wrap(~ age) +
  geom_violin() +
  scale_y_continuous(trans='pseudo_log') +
  scale_fill_brewer(palette="Pastel2") +
  theme_light() 
dev.off()

jpeg("output/conAllAgesByUrban.jpg", width=1400, height=680)
ggplot(data = tidySpp, aes(x = age, y = count, fill = Urban)) +
  geom_violin() +
  scale_y_continuous(trans='pseudo_log') +
  scale_fill_brewer(palette="Pastel2") +
  theme_light() +
  ylab(bquote('Conifer Number in sampled 30 m' ^2)) 
dev.off()


