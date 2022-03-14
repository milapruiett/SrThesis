# are there fewer nurse logs

wood<-read_csv("data/wood.csv")

# first look at all wood
urban <- c("Lacamas", "FP", "RVNA", "Marquam", "Tryon")
rural <- c("McIver", "Oxbow", "Wildwood", "Sandy", "Barlow")

wood <- wood %>% 
  mutate(Urban=case_when(Forest %in% urban ~ 'urban', Forest %in% rural ~ 'rural', TRUE ~ NA_character_))

sumWood <- wood %>% 
  group_by(Urban, Forest, Plot) %>% 
  summarize(a=sum(SA))

write.csv(sumWood, "data/sumWoodAllDecayClasses.csv")

#ensure forest is as a factor
sumWood$Forest <- factor(sumWood$Forest , levels=c(
  "Barlow", "McIver", "Oxbow", "Sandy", "Wildwood",
  "FP", "Lacamas", "Marquam", "RVNA",   "Tryon" ))

jpeg("output/allLogsByForest.jpeg")
ggplot(data = sumWood, aes(x = Forest, y = (1/1500)*a, fill = Urban)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab(bquote('Logs of all decay classes, surface area in sq cm per sq m forest surveyed')) + 
  xlab("") +
  ggtitle("Logs of all decay classes in urban and rural forests")
dev.off()

# test for significance
summary(aov(log10(1 + a) ~ Urban / Forest, data = sumWood))
t.test(log10(1 + sumWood$a) ~ sumWood$Urban)

# get the mean of each group to report out
sumWood %>%
  group_by(Urban) %>%
  summarise(mean(a))

# collapse data
avgWood <- wood %>% 
  group_by(Forest, Urban) %>% 
  summarize(sumWood=sum(SA))

jpeg("output/allLogsByUrban.jpg")
ggplot(data = avgWood, aes(x = Urban, y = ((1/1500)*sumWood), fill=Urban)) +
  geom_boxplot() +
  scale_y_continuous(trans='log10') +
  scale_fill_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab("Sum of Sq cm logs of all decay classes per sq m sampled") + 
  xlab("") +
  ggtitle("Rural forests seem to have more logs total than urban forests")
dev.off()

#wilcox test
wilcox.test(avgWood$sumWood ~ avgWood$Urban)


#only want decay classes 4 and 5

wood45=subset(wood, DecayClass %in% c("1", "2", "3"))

#need to add a column to signify if urban or rural
urban <- c("Lacamas", "FP", "RVNA", "Marquam", "Tryon")
rural <- c("McIver", "Oxbow", "Wildwood", "Sandy", "Barlow")

wood45 <- wood45 %>% 
  mutate(Urban=case_when(Forest %in% urban ~ 'urban', Forest %in% rural ~ 'rural', TRUE ~ NA_character_))

sumWood45 <- wood45 %>% 
  group_by(Urban, Forest, Plot) %>% 
  summarize(a=sum(SA))

forestName <- unique(sumWood$Forest)
completeWood <- data.frame(Urban=character(),
                           Forest=character(), 
                           Plot=numeric(), 
                           a= numeric(),
                           stringsAsFactors=FALSE)

completeWood <- tibble(completeWood)

#add zeros so that each forest has 10 plots, with the sum of the area of nurse log
#in each plot
for (forest in forestName) {
 sumByForest <- sumWood45 %>% filter(Forest == forest)
 dummiesNeeded <- (10-nrow(sumByForest))
 highestNum <- max(as.numeric(sumByForest$Plot))
 completeWood <- bind_rows(completeWood, sumByForest)
 U <- unique(sumByForest$Urban)[1]
 f <- unique(sumByForest$Forest)[1]
 numDum <- highestNum + dummiesNeeded 
 value <- highestNum +1
 print(value: numDum)
 for (ID in value: numDum) {
   completeWood <- add_row(completeWood, Urban = U, Forest = f, Plot = ID, a=0)
 
  }
}

#ensure forest is as a factor
completeWood$Forest <- factor(completeWood$Forest , levels=c(
  "Barlow", "McIver", "Oxbow", "Sandy", "Wildwood",
  "FP", "Lacamas", "Marquam", "RVNA", 
  "Tryon"))

write.csv(completeWood, "data/completeWoodDecay45.csv")

jpeg("output/wood45ByForest.jpg")
ggplot(data = completeWood, aes(x = Forest, y = (1/1500)*a, fill = Urban)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab(bquote('Logs of decay classes 4 and 5, surface area in sq cm per sq m forest surveyed')) + 
  xlab("") +
  ggtitle("Logs of decay classes 4 and 5 in urban and rural forests")
dev.off()

# test for significance
summary(aov(log10(1 + a) ~ Urban / Forest, data = completeWood))

# see the mean by group
completeWood %>%
  group_by(Urban) %>%
  summarise(mean(a))

# collapse data of decy 4 and 5
avgWood45 <- wood45 %>% 
  group_by(Forest, Urban) %>% 
  summarize(sumWood=sum(SA))

jpeg("output/Logs45ByUrban.jpg")
ggplot(data = avgWood45, aes(x = Urban, y = ((1/1500)*sumWood), fill=Urban)) +
  geom_boxplot() +
  scale_y_continuous(trans='log10') +
  scale_fill_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab("Sum of Sq cm logs of decay classes 4 and 5 per sq m sampled") + 
  xlab("") +
  ggtitle("Sum of wood of decay classes 4 and 5 in urban and rural forests")
dev.off()

#t.test 
wilcox.test(avgWood45$sumWood~avgWood45$Urban)
