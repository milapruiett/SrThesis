# are there fewer nurse logs
wood<-read_csv("wood.csv")
# row 254 is missing data
wood$Forest[254] = "Wildwood"

#only want decay classes 4 and 5
wood45=subset(wood, DecayClass %in% c("4", "5"))

#need to add a column to signify if urban or rural
urban <- c("Lacamas", "FP", "RVNA", "Marquam", "Tryon")
rural <- c("McIver", "Oxbow", "Wildwood", "Sandy", "Barlow")

wood45 <- wood45 %>% 
  mutate(Urban=case_when(Forest %in% urban ~ 'urban', Forest %in% rural ~ 'rural', TRUE ~ NA_character_))

sumWood <- wood45 %>% 
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
 sumByForest <- sumWood %>% filter(Forest == forest)
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

write.csv(completeWood, "completeWood.csv")

ggplot(data = completeWood, aes(x = Forest, y = (1/1500)*a, fill = Urban)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab(bquote('Logs of decay classes 4 and 5, surface area in sq cm per sq m forest surveyed')) + 
  xlab("anova p-value on log transformed data is 0.12") +
  ggtitle("Each boxplot represents 10 transects, only decay classes 4 and 5")


summary(aov(log10(1 + a) ~ Urban / Forest, data = completeWood))

# collapse data
avgWood <- wood45 %>% 
  group_by(Forest, Urban) %>% 
  summarize(sumWood=sum(SA))

ggplot(data = avgWood, aes(x = Urban, y = ((1/1500)*sumWood), fill=Urban)) +
  geom_boxplot() +
  scale_y_continuous(trans='log10') +
  scale_fill_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab("Sum of Sq cm logs of decay classes 4 and 5 per sq m sampled") + 
  xlab("wilcox test, p-value = 0.5 ") +
  ggtitle("Rural forests seem to have more logs total than urban forests")

#t.test 
hist(log10(1+(1/1500)*avgWood$sumWood))

ggplot(data = avgWood, aes(x= sumWood, fill = Urban)) +
  geom_histogram()+ 
  scale_fill_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ggtitle("Data is maybe normal? Maybe Not?")

t.test(avgWood$sumWood~avgWood$Urban)
wilcox.test(avgWood$sumWood~avgWood$Urban)
