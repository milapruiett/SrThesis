# are there fewer decidious trees?
tidySpp <- read_csv("data/tidySpp.csv")

tidySpp$age <- factor(tidySpp$age , levels=c("g", "s", "sm", "lg", "can"))

jpeg("output/morphAgePseudolog.jpg")
ggplot(data = tidySpp, aes(x = age, y = count, fill=Urban)) + 
  geom_boxplot() + facet_wrap(~ morph, scale="free") + 
  scale_y_continuous(trans='pseudo_log') +
  scale_fill_brewer(palette="Pastel2") +
  theme_light()
dev.off()

jpeg("output/morphAgeLog.jpg")
ggplot(data = tidySpp, aes(x = age, y = count, fill=morph)) + 
  geom_boxplot() + facet_wrap(~ Urban, scale="free") + 
  scale_y_continuous(trans='pseudo_log') +
  scale_fill_brewer(palette="Pastel1") +
  theme_light()
dev.off()

jpeg("output/UrbanFillMorph.jpg")
ggplot(data = tidySpp, aes(x = Urban, y = count, fill=morph)) + 
  geom_boxplot() + facet_wrap(~ age, scale="free") + 
  scale_y_continuous(trans='pseudo_log') +
  scale_fill_brewer(palette="Pastel1") +
  theme_light()
dev.off()

# by site
jpeg("output/MorphAgeBySite.jpg")
ggplot(data = tidySpp, aes(x = age , y = count, fill=morph)) + 
  geom_boxplot() + facet_wrap(~ SiteName , scale="free") + 
  scale_y_continuous(trans='pseudo_log') +
  scale_fill_brewer(palette="Pastel1") +
  theme_light()
dev.off()

summary(aov(count ~ Urban * age * morph, data = tidySpp))


tidyAgeSummary <- tidySpp %>% 
  group_by(morph, SiteName, Urban, age) %>% 
  summarize(sum=sum(count, na.rm=TRUE))

jpeg("output/SumUrbanMorph.jpg")
ggplot(data = tidyAgeSummary, aes(x = Urban, y = sum, fill=morph)) + 
  geom_boxplot() + facet_wrap(~ age, scale="free") + 
  scale_fill_brewer(palette="Pastel1") +
  theme_light()
dev.off()

summary(aov(sum ~ Urban * age * morph, data = tidyAgeSummary))

summary(aov(tidySpp$count ~ tidySpp$Urban * tidySpp$age * tidySpp$species))

tidySpp$species <- factor(tidySpp$species, levels=c("ACCI", "ACMA", "ALRU", "QUGA",
                                                    "AB", "TSHE", "THPL", "PSME"))

ggplot(tidySpp, aes(fill=species, y=count, x=Urban)) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~ age, scale="free") +
  scale_fill_brewer(palette="RdYlBu") 



