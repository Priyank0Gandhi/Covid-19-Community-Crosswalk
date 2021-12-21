library(tidyverse)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(GGally)
library(coefplot)
library(corrplot)
library(tmap)
library(tmaptools)
library(sf)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(maps)
library(mapproj)
library(mapdata)
library(ggmap)
library(usmap)
library(micromapST)
library(mltools)
library(data.table)
install.packages("C:/Users/PRIYANK GANDHI/Downloads\\fiftystater_1.0.1.tar.gz",
                 repos = NULL, type = "source")
library(fiftystater)

#reading the data in:
d<-read_csv('C:/Users/PRIYANK GANDHI/Desktop/AIT-580/Final Project/project.csv')
d[d$`Total Score`=="Manual Verification Required",]<- NA
sum(is.na(d$`Total Score`))
d %>%
  mutate(across(c(`Total Score`),na_if, "Manual Verification Required"))
is.na(d) <- d == "Manual Verification Required"

table(d$`Low Income Area (LIA) County SAIPE - (Poverty Percentage)`)
summary(d$`Total Score`)
sapply(d, class)

sum(is.na(d$`Low Income Area (LIA) County SAIPE - (Poverty Percentage)`))
d<-na.omit(d)

#Conversion of classes:
d$`Hardest Hit Area (HHA)`=as.factor(d$`Hardest Hit Area (HHA)`)
u<-unique(d[c("STATE", "ST_ABBR")])
names(d)

#One Hot encoding:
doh<- one_hot(as.data.table(d))

d$ST_ABBR=as.factor(d$ST_ABBR)
d$`Tribal Community\n(1 if yes)`= as.factor(d$`Tribal Community\n(1 if yes)`)


dr<-(d$Rural==1)
dnr<- d[!dr,]
dr<-d[dr,]

#Preparing dataframes:
d_reg = d %>% group_by(STATE,ST_ABBR)  %>%
  summarise( HHA= mean(`HHA Score`),
             LIASAIPE = mean(`Low Income Area (LIA) County SAIPE- Score`),
             LIACT= mean(`Low Income Area (LIA) Census Tract - Score`),
             RuralScore= mean(`Rural - Score`),
             LIASAIPEperc=mean(`Low Income Area (LIA) County SAIPE - (Poverty Percentage)`),
             LIACTperc=mean(`Low Income Area (LIA) Census Tract (Poverty Percentage)`),
             TS=mean(`Max Possible Score`),
             .groups = 'drop')
names(d)

sapply(d_hha,  class)

d_hha= doh %>% group_by(STATE,ST_ABBR)  %>%
  summarise( SH= sum(`Hardest Hit Area (HHA)_SustainedHotspot`),
             H = sum(`Hardest Hit Area (HHA)_Hotspot`),
             MB= sum(`Hardest Hit Area (HHA)_ModerateBurden`),
             LB= sum(`Hardest Hit Area (HHA)_LowBurden`),
             MBR=sum(`Hardest Hit Area (HHA)_ModerateBurdenResolving`),
             HBR=sum(`Hardest Hit Area (HHA)_HighBurdenResolving`),
             EH=sum(`Hardest Hit Area (HHA)_EmergingHotspot`),
             .groups = 'drop')


is.na(row.names(d_hha))
d_hha_tb=as.tibble(d_hha)
d_hha_tb$STATE<-tolower(d_hha_tb$STATE)
d_hha$STATE<-tolower(d_hha$STATE)
colnames(d_hha_tb)[1]= 'region'
colnames(d_hha)[1]= 'region'

#Individual Score Analysis For Every State:
ggplot(d_reg, aes(y =reorder(`ST_ABBR`, `HHA`), x=`HHA`, fill=STATE)) +
  geom_bar(stat = 'identity', color='black')+labs(title= "Hardest Hit Area(HHA) Score vs States",
                                                  y="States", x = "HHA score")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="none",
        axis.text.y = element_text(color = "black", size =7, angle = 0, hjust = 1,
                                   vjust = 0, face = "plain"))


ggplot(d_reg, aes(y =reorder(`ST_ABBR`, `LIASAIPE`), x=`LIASAIPE`, fill=STATE)) +
  geom_col(stat = 'identity', color='black')+labs(title= "LOW Income Area(SAIPE) vs States",
                                                  y="States", x = "LIA(SAIPE) score")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="none",
        axis.text.y = element_text(color = "black", size =7, angle = 0, hjust = 1,
                                   vjust = 0, face = "plain"))

ggplot(d_reg, aes(y =reorder(`ST_ABBR`, `LIACT`), x=`LIACT`, fill=STATE)) +
  geom_bar(stat = 'identity', color='black')+labs(title= "Low Income Area(CT)Score vs States",
                                                  y="States", x = "LIA(CT) score")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="none",
        axis.text.y = element_text(color = "black", size =7, angle = 0, hjust = 1,
                                   vjust = 0, face = "plain"))

ggplot(d_reg, aes(y =reorder(`ST_ABBR`, `TS`), x=`TS`, fill=STATE)) +
  geom_bar(stat = 'identity', color='black')+labs(title= "Mean of Total Score vs States",
                                                  y="States", x = "Total score")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="none",
        axis.text.y = element_text(color = "black", size =7, angle = 0, hjust = 1,
                                   vjust = 0, face = "plain"))

ggplot(d_reg, aes(y =reorder(`ST_ABBR`, `RuralScore`), x=`RuralScore`, fill=STATE)) +
  geom_bar(stat = 'identity', color='black')+labs(title= "RuralScore vs States",
                                                  y="States", x = "Rural score")+
  theme(plot.title = element_text(hjust = 0.5),legend.position="none",
        axis.text.y = element_text(color = "black", size =7, angle = 0, hjust = 1,
                                   vjust = 0, face = "plain"))


#Cloropleth Map:
us<-map_data("state")
dh <- us %>%
  left_join(d_hha, by='region')
dh$region<-factor(dh$region)
dh <- dh[order(dh$order),]


centroids <- data.frame(region=tolower(state.name), long=state.center$x, lat=state.center$y)
centroids$abb<-state.abb[match(centroids$region,tolower(state.name))]
statenames<-data.frame(
  region=levels(dh$region)
) 
# Merge it with centroids
centroids<-merge(statenames,centroids,by="region")

p1 <- ggplot(d_hha, aes(map_id = region)) + 
  geom_map(aes(fill = SH), map = fifty_states, color='black')+
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map()+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0.25)+
  theme_minimal()+
  labs(
    title = 'Sustained Hotspots',
    x="", y="", fill=""
  )+
  theme(
    plot.title = element_text(size=18, face='bold', color = 'black')
  )+
  with(centroids, 
       annotate(geom="text", x = long, y=lat, label = abb, 
                size = 2.5,color="black",family="Times")
  )+
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(panel.background = element_blank()) +
  fifty_states_inset_boxes()
p1

p2 <- ggplot(d_hha, aes(map_id = region)) + 
  geom_map(aes(fill = H), map = fifty_states, color='black')+
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map()+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 750)+
  theme_minimal()+
  labs(
    title = 'Hotspots',
    x="", y="", fill=""
  )+
  theme(
    plot.title = element_text(size=18, face='bold', color = 'black')
  )+
  with(centroids, 
       annotate(geom="text", x = long, y=lat, label = abb, 
                size = 2.5,color="black",family="Times")
  )+
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(panel.background = element_blank()) +
  fifty_states_inset_boxes()
p2

p3 <- ggplot(d_hha, aes(map_id = region)) + 
  geom_map(aes(fill = MB), map = fifty_states, color='black')+
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map()+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 1000)+
  theme_minimal()+
  labs(
    title = 'Moderate Burden',
    x="", y="", fill=""
  )+
  theme(
    plot.title = element_text(size=18, face='bold', color = 'black')
  )+
  with(centroids, 
       annotate(geom="text", x = long, y=lat, label = abb, 
                size = 2.5,color="black",family="Times")
  )+
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(panel.background = element_blank()) +
  fifty_states_inset_boxes()
p3

p4 <- ggplot(d_hha, aes(map_id = region)) + 
  geom_map(aes(fill = LB), map = fifty_states, color='black')+
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map()+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 37.5)+
  theme_minimal()+
  labs(
    title = 'Low Burden',
    x="", y="", fill=""
  )+
  theme(
    plot.title = element_text(size=18, face='bold', color = 'black')
  )+
  with(centroids, 
       annotate(geom="text", x = long, y=lat, label = abb, 
                size = 2.5,color="black",family="Times")
  )+
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(panel.background = element_blank()) +
  fifty_states_inset_boxes()
p4

p5 <- ggplot(d_hha, aes(map_id = region)) + 
  geom_map(aes(fill = MBR), map = fifty_states, color='black')+
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map()+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 400)+
  theme_minimal()+
  labs(
    title = 'Moderate Burden Resolving',
    x="", y="", fill=""
  )+
  theme(
    plot.title = element_text(size=18, face='bold', color = 'black')
  )+
  with(centroids, 
       annotate(geom="text", x = long, y=lat, label = abb, 
                size = 2.5,color="black",family="Times")
  )+
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(panel.background = element_blank()) +
  fifty_states_inset_boxes()
p5

p6 <- ggplot(d_hha, aes(map_id = region)) + 
  geom_map(aes(fill = HBR), map = fifty_states, color='black')+
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map()+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 400)+
  theme_minimal()+
  labs(
    title = 'High Burden Resolving',
    x="", y="", fill=""
  )+
  theme(
    plot.title = element_text(size=18, face='bold', color = 'black')
  )+
  with(centroids, 
       annotate(geom="text", x = long, y=lat, label = abb, 
                size = 2.5,color="black",family="Times")
  )+
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(panel.background = element_blank()) +
  fifty_states_inset_boxes()
p6

p7 <- ggplot(d_hha, aes(map_id = region)) + 
  geom_map(aes(fill = EH), map = fifty_states, color='black')+
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map()+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0.25)+
  theme_minimal()+
  labs(
    title = 'Emerging Hotspots',
    x="", y="", fill=""
  )+
  theme(
    plot.title = element_text(size=18, face='bold', color = 'black')
  )+
  with(centroids, 
       annotate(geom="text", x = long, y=lat, label = abb, 
                size = 2.5,color="black",family="Times")
  )+
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(panel.background = element_blank()) +
  fifty_states_inset_boxes()
p7


#Correlation Plot:
vars <- dplyr::select(d_reg,`HHA`, everything(), -c(STATE,ST_ABBR, TS))
corrplot(cor(vars[sapply(vars, function(x) !is.factor(x))]),type="upper", method="color", diag=FALSE,
         tl.srt=30, addCoef.col="black", main="Correlation Plot")


#Relationship Between Poverty percent and HHA scores:
ggplot(d_reg, aes(x=`HHA` ,y=`LIACTperc`)) +
  geom_point()+geom_smooth()+
  labs(x="HHA Score",
       y="Poverty (CT) percent",
       title="Relationship Between Poverty percent(CT) and HHA scores")

#Relationship Between Poverty percent and HHA scores: 
ggplot(d_reg, aes(x=`HHA` ,y=`LIASAIPEperc`)) +
  geom_point()+geom_smooth()+
  labs(x="HHA Score",
       y="Poverty (SAIPE) percent",
       title="Relationship Between Poverty percent(SAIPE) and HHA scores")





