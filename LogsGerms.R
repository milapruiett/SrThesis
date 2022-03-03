# do nurse logs line up with germinants?

conG <- read_csv("conG.csv")
completeWood <- read_csv("completeWood.csv")

# summarize

conGermSummary <- conG %>% 
  group_by(Urban, SiteName) %>% 
  summarize(meanG=mean(CONg, na.rm=TRUE), seG = std.error(CONg, na.rm=TRUE))

woodSummary <- completeWood %>% 
  group_by(Urban, Forest) %>% 
  summarize(meanW=mean(a, na.rm=TRUE), seW = std.error(a, na.rm=TRUE))

# rename so all forests have same names
woodSummary$Forest <- c("Barlow", "McIver", "Oxbow", "Sandy", 
                           "Wildwood", "ForestPark", "Lacamas", 
                           "Marquam", "Riverview", "Tryon")

# inner_join()
woodGerm= conGermSummary %>% inner_join(woodSummary,by=c("SiteName" ="Forest"))

# plot
ggplot(data = woodGerm, aes(x = meanW, y = meanG)) + geom_point()
