# are there fewer nurse logs

wood<-read_csv("data/wood.csv")

# there is a log with the wrong value for decay class
wood$DecayClass[257] = 5

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
  scale_y_continuous(trans='pseudo_log') +
  ggtitle("")+
  theme(legend.title = element_blank()) +
  ylab(bquote('Surface area for all logs per transect' (cm/m)^2)) + 
  xlab("") +
  theme(text=element_text(size=15))
dev.off()

# test for significance
summary(aov(a ~ Urban / Forest, data = sumWood))
t.test(sumWood$a ~ sumWood$Urban)

# get the mean of each group to report out
sumWood %>%
  group_by(Urban) %>%
  summarise((1/1500) *mean(a), (1/1500) *std.error(a))

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

wood45=subset(wood, DecayClass %in% c("4", "5"))

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
  "FP", "Lacamas", "Marquam", "RVNA", 
  "Tryon", "Barlow", "McIver", "Oxbow", "Sandy", "Wildwood"))

completeWood$Forest <- recode_factor(completeWood$Forest, RVNA = "Riverview", FP = "Forest Park")

write.csv(completeWood, "data/completeWoodDecay45.csv")

jpeg("output/wood45ByForest.jpg")
ggplot(data = completeWood, aes(x = Forest, y = (1/1500)*a, fill = Urban)) +
  geom_boxplot(outlier.shape=21) +
  scale_fill_manual(values = c("#fdcdac", "#b3e2cd")) +
  theme_light() +
  theme(text=element_text(size=15)) +
  scale_y_continuous(trans = "pseudo_log") + 
  theme(legend.title = element_blank()) +
  ylab(bquote('Surface area for logs decay 4 and 5 per transect' (cm/m)^2)) + 
  xlab("") +
  ggtitle("")
dev.off()

# test for significance
summary(aov(log10(1 + a) ~ Urban / Forest, data = completeWood))


# what percent of the data are zeros
100*sum(completeWood == 0)/nrow(completeWood) # = 68% of the plots are 0s

# see the mean by group
completeWood %>%
  group_by(Urban) %>%
  summarise(1/1500 * mean(a), 1/1500 * std.error(a))

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

# what is the relationship between logs and decay 
woodDecay <- wood %>% 
  group_by(Urban, Forest, DecayClass) %>% 
  summarize(a=sum(SA))

ggplot(data = woodDecay, aes(y = a, x = Urban, color=Urban)) +
  facet_wrap(~ DecayClass) +
  geom_boxplot() +
  scale_y_continuous(trans='log10') +
  scale_color_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) 

ggplot(data = wood, aes(y = AvgDepth, x = Forest, color=Urban)) +
  geom_boxplot() +
  scale_y_continuous(trans='log10') +
  scale_color_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) 

ggplot(data = wood, aes(y = AvgDepth, x = Forest, color=Urban)) +
  geom_point() +
  scale_y_continuous(trans='log10') +
  scale_color_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) 


wood <- wood %>% filter(SA >0)

summary(lm(AvgDepth ~  Urban + Forest, data = wood))


# does the amount of wood vary by decay class and urban status?
decayWood <- wood %>% 
  group_by(Urban, Forest,DecayClass) %>% 
  summarize(a=sum(SA)) %>% 
  ungroup()

# remove decay class 0
decayWood <- filter(decayWood, DecayClass > 0)

# not every forest has a log of every decay class, add those in
## Marquam 5, FP 3 5, Wildwood 5, Oxbow 5, 
decayWood <- decayWood %>% add_row(Urban = "urban", Forest = "Marquam", DecayClass = 5, a = 0)
decayWood <- decayWood %>% add_row(Urban = "urban", Forest = "FP", DecayClass = 5, a = 0)
decayWood <- decayWood %>% add_row(Urban = "urban", Forest = "FP", DecayClass = 3, a = 0)
decayWood <- decayWood %>% add_row(Urban = "rural", Forest = "Wildwood", DecayClass = 5, a = 0)
decayWood <- decayWood %>% add_row(Urban = "rural", Forest = "Oxbow", DecayClass = 5, a = 0)

summary(aov(a ~ Urban, data = decayWood[decayWood$DecayClass=="1", ]))
summary(aov(a ~ Urban, data = decayWood[decayWood$DecayClass=="2", ]))
summary(aov(a ~ Urban, data = decayWood[decayWood$DecayClass=="3", ]))
summary(aov(a ~ Urban, data = decayWood[decayWood$DecayClass=="4", ]))
summary(aov(a ~ Urban, data = decayWood[decayWood$DecayClass=="5", ]))


summary(aov(a ~ Urban * DecayClass, data = decayWood))

decayWood$DecayClass <- as.factor(decayWood$DecayClass)
decayWood$Urban <- factor(decayWood$Urban, levels=c("urban", "rural"))

ggplot(data = decayWood, aes(y = a / 15000, x = DecayClass, fill=Urban)) +
  geom_boxplot(outlier.shape=21) +
  scale_fill_manual(values = c("#fdcdac", "#b3e2cd")) +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab(bquote("Sum of log surface area within a site" (cm /m ) ^ 2)) + 
  xlab("Decay Class") +
  ggtitle("")

decayWood %>%
  group_by(Urban, DecayClass) %>%
  summarise(1/15000 * mean(a), 1/15000 * std.error(a))
