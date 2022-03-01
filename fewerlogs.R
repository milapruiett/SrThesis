# are there fewer nurse logs
wood<-read_csv("wood.csv")

#only want decay classes 4 and 5
wood45=subset(wood, DecayClass %in% c("4", "5"))

#need to add a column to signify if urban or rural
urban <- c("Lacamas", "FP", "RVNA", "Marquam", "Tryon")
rural <- c("McIver", "Oxbow", "Wildwood", "Sandy", "Barlow")

wood45 <- wood45 %>% 
  mutate(Urban=case_when(Forest %in% urban ~ 'urban', Forest %in% rural ~ 'rural', TRUE ~ NA_character_))

#ensure forest is as a factor
wood45$Forest <- factor(wood45$Forest , levels=c(
  "FP", "Lacamas", "Marquam", "RVNA", 
  "Tryon", "Barlow", "McIver", "Oxbow", "Sandy", "Wildwood"))

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



















ggplot(data = wood45, aes(x = Forest, y = SA, fill = Urban)) +
  geom_boxplot() +
  scale_y_continuous(trans='log10')+
  scale_fill_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab(bquote('Nurse Log Surface Area in 150 m'^2)) + 
  xlab(" ") +
  ggtitle("Rural forests seem have more nurse logs than urban forests")

#two-way ANOVA with wood data
ggplot(data = wood45, aes(x= SA, fill = Urban)) +
  geom_histogram()+ 
  scale_fill_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ggtitle("Data is not normally distributed")

ggplot(data = wood45, aes(x= log10(1+SA), fill = Urban)) +
  geom_histogram()+ 
  scale_fill_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ggtitle("Data is more normal when log-transformed")


summary(aov(log10(1+SA) ~ Urban / Forest, data = wood45))

# collapse data
avgWood <- wood45 %>% 
  group_by(Forest, Urban) %>% 
  summarize(a=sum(SA), se=std.error(SA))

ggplot(data = avgWood, aes(x = Urban, y = (1+(1/1500)*a), fill=Urban)) +
  geom_boxplot() +
  scale_y_continuous(trans='log10') +
  scale_fill_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ylab("Sum of Sq cm nurse log per sq m sampled") + 
  xlab(" ") +
  ggtitle("Rural forests do not have more nurse logs than urban forests")

#t.test 
hist(log10(1+(1/1500)*avgWood$a))

ggplot(data = avgWood, aes(x= a, fill = Urban)) +
  geom_histogram()+ 
  scale_fill_brewer(palette="Pastel2") +
  theme_light() +
  theme(legend.title = element_blank()) +
  ggtitle("Data is maybe normal? Maybe Not?")

t.test(log10(1+(1/1500)*avgWood$a)~avgWood$Urban)
wilcox.test(avgWood$a~avgWood$Urban)
