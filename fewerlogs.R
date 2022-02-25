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

ggplot(data = wood45, aes(x = Forest, y = SA, fill = Urban)) +
  geom_boxplot() 


boxplot(.001*woodcombo2$SA~woodcombo2$Urban,
        main="Rural forests generally have more decayed wood",
        xlab="Wilcoxon test, p=0.14",
        col=c("#B8D685", "#FFCA85"),
        ylab="Total decayed wood in sq m per forest"
) 
# want to log transform the sa?
woodcombo2$lSA<-log10(1+(1/1500)*woodcombo2$SA)
t.test(log10(1+(1/1500)*woodcombo2$SA)~woodcombo2$Urban)
#what about just graphing it with a log transformed axis?
boxplot(I(1+(1/1500)*woodcombo2$SA)~woodcombo2$Urban, log="y",
        col=c("#B8D685", "#FFCA85"),
        ylab="Sq cm nurse log per sq m sampled", 
        main="Rural forests generally have more decayed wood",
        xlab="T-test, p=0.11") 