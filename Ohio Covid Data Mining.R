# Author: Ibrahim Sanusi
# The following packages were loaded to complete the project 
library(tidyverse)
library(lubridate)
library(tidyquant)
library(patchwork)
library(RColorBrewer)
library(readr)
library(maps)             
library(ggplot2)
library(ggmap)            
library(mapproj)
library(ggthemes)
library(gapminder)
library(gridExtra)
library(ggmosaic)
library(ggrepel)
library(patchwork)

#...........................Data Preparation..........................#
OhioCovidData2 <- read_csv(
  file="https://coronavirus.ohio.gov/static/dashboards/COVIDDeathData_CountyOfResidence.csv",
  col_names = c("County","Sex","AgeRange","OnsetDate","HospDate",
                "DeathDate","CaseCount","HospCount",
                "DeathCount"),
  col_types = "cccDDDiii",
  skip=1)
str(OhioCovidData2)
dim(OhioCovidData2)
sum(is.na(OhioCovidData2$DeathDate))
view(OhioCovidData2)	

#............................ removing unknown........................#
OhioCovidData2 <- OhioCovidData2 %>%
  filter(County != "Unknown")

#............................ Grouping by county and finding sum of cases........................#
OhioTotalCase2 <- OhioCovidData2 %>%
  group_by(County) %>%
  summarize(Cases = sum(CaseCount))
OhioTotalHosp2<- OhioCovidData2 %>%
  group_by(County) %>%
  summarize(Hosps = sum(HospCount))
OhioTotalDeath2 <- OhioCovidData2 %>%
  group_by(County) %>%
  summarize(Deaths = sum(DeathCount))

#............................ Inner joining to tidy up the data set .......................#
OhioTotal2 <-OhioTotalCase2 %>%
  inner_join(OhioTotalHosp2) %>%
  inner_join(OhioTotalDeath2)
head(OhioTotal2)

#..............................Grouping Cases by Onset Date................................#
OhioCovidCountGroup <- OhioCovidData2 %>% 
  group_by(OnsetDate) %>% 
  summarise(totalCount = sum(`CaseCount`))
head(OhioCovidCountGroup)

OhioCovidData2 %>%
  summarize(TotalCases = sum(`CaseCount`))

#............................ Plotting Time series bar graph for Cases ........................#
ggplot(data=OhioCovidCountGroup, aes(`OnsetDate`,`totalCount`)) +
  geom_col(color = "steelblue")+ 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %d, %y")+
  annotate("text", x=mdy("Aug 13 20"), y=13000, label="Cases", size = 8)+
  annotate("text", x=mdy("Aug 13 20"), y=7000, label="1,026,929", size = 12, color="coral4")+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "azure", color="azure"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="", y="")

g1_Case1<-ggplot(data=OhioCovidCountGroup, aes(`OnsetDate`,`totalCount`)) +
  geom_col(color = "steelblue")+ 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %d, %y")+
  annotate("text", x=mdy("Aug 13 20"), y=13000, label="Cases", size = 8)+
  annotate("text", x=mdy("Aug 13 20"), y=7000, label="1,026,929", size = 12, color="coral4")+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "azure", color="azure"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="", y="")

#............................ Plotting Time series bar graph for Hospitalization ........................#  
OhioCovidHospGroup <- OhioCovidData2 %>% 
  group_by(OnsetDate) %>% 
  summarise(HospitalizedCount = sum(`HospCount`))
head(OhioCovidHospGroup)
view(OhioCovidHospGroup)

OhioCovidData2 %>%
  summarize(TotalCases = sum(`HospCount`))
  ggplot(data=OhioCovidHospGroup, aes(`OnsetDate`,`HospitalizedCount`, yaxt = "n")) +
  geom_col(color = "steelblue") + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %d, %y")+
  annotate("text", x=mdy("Aug 13 20"), y=500, label="Hospitalization", size = 8)+
  annotate("text", x=mdy("Aug 13 20"), y=250, label="53,445", size = 12, color="coral4")+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "azure", color="azure"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="", y="")

g1_Hosp1<-ggplot(data=OhioCovidHospGroup, aes(`OnsetDate`,`HospitalizedCount`, yaxt = "n")) +
  geom_col(color = "steelblue") + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %d, %y")+
  annotate("text", x=mdy("Aug 13 20"), y=500, label="Hospitalization", size = 8)+
  annotate("text", x=mdy("Aug 13 20"), y=250, label="53,445", size = 12, color="coral4")+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "azure", color="azure"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="", y="")

#............................ Plotting Time series bar graph for Death ........................#  
OhioCovidDeathGroup <- OhioCovidData2 %>% 
  group_by(DeathDate) %>% 
  summarise(DeathCount = sum(`DeathCount`))
head(OhioCovidDeathGroup)
view(OhioCovidDeathGroup)

OhioCovidData2 %>%
  summarize(DeathCount = sum(`DeathCount`))
  ggplot(data=OhioCovidDeathGroup, aes(`DeathDate`,`DeathCount`, yaxt = "n")) +
  geom_col(color = "steelblue") + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %d, %y")+
  annotate("text", x=mdy("Aug 13 20"), y=500, label="Hospitalization", size = 8)+
  annotate("text", x=mdy("Aug 13 20"), y=250, label="53,445", size = 12, color="coral4")+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "azure", color="azure"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="", y="")

g1_Death1<-ggplot(data=OhioCovidDeathGroup, aes(`DeathDate`,`DeathCount`, yaxt = "n")) +
  geom_col(color = "steelblue") + 
  scale_x_date(date_breaks = "3 month", date_labels = "%b %d, %y")+
  annotate("text", x=mdy("Aug 13 20"), y=500, label="Hospitalization", size = 8)+
  annotate("text", x=mdy("Aug 13 20"), y=250, label="53,445", size = 12, color="coral4")+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "azure", color="azure"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="", y="")

#......................................Bottom Bar Charts..................................#
OhioTotal2 %>%
  ggplot(aes(x=Cases,y=fct_reorder(County, Cases))) +
  geom_col()

#............................ Considering the first 20 County  .......................#
OhioTotal2 %>%
  arrange(by_group = desc(Cases))
oo <- order(OhioTotal2$Cases,decreasing=TRUE)
Counties20 <- as.vector(OhioTotal2$County[oo])[1:20]
Counties20

#............................ Cases .......................#
OhioTotal2 %>%
  filter(County %in% Counties20) %>%
  group_by(Cases)%>%
  ggplot(aes(x=Cases,y=fct_reorder(County, Cases), fill=(Cases)))+
  geom_col()+
  geom_text(aes(label = Cases), vjust = 0.5, hjust=-0.1, size = 3)+
  expand_limits(x = 120000, y = 0)+
  scale_fill_gradient(low="lightblue", high = "steelblue")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title="Covid Cases by Age Group", x="", y="")+
  ggtitle("Case Count")+
  theme(legend.position = (""))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = "azure", color="azure"))

g2_Case2<-OhioTotal2 %>%
  filter(County %in% Counties20) %>%
  group_by(Cases)%>%
  ggplot(aes(x=Cases,y=fct_reorder(County, Cases), fill=(Cases)))+
  geom_col()+
  geom_text(aes(label = Cases), vjust = 0.5, hjust=-0.1, size = 3)+
  expand_limits(x = 120000, y = 0)+
  scale_fill_gradient(low="lightblue", high = "steelblue")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title="Covid Cases by Age Group", x="", y="")+
  ggtitle("Case Count")+
  theme(legend.position = (""))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = "azure", color="azure"))

# ..........................Hospitalization ...................#
OhioTotal2 %>%
  filter(County %in% Counties20) %>%
  ggplot(aes(x=Hosps,y=fct_reorder(County, Cases), fill=(Hosps)))+
  geom_col()+
  geom_text(aes(label = Hosps), vjust = 0.5, hjust=-0.1, size=3)+
  expand_limits(x = 5000, y = 0)+
  scale_fill_gradient(low="lightblue", high = "steelblue")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title="Covid Cases by Age Group", x="", y="")+
  ggtitle("Hospitalization Count")+
  theme(legend.position = (""))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+        
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = "azure", color="azure"))

g2_Hosp2<-OhioTotal2 %>%
  filter(County %in% Counties20) %>%
  ggplot(aes(x=Hosps,y=fct_reorder(County, Cases), fill=(Hosps)))+
  geom_col()+
  geom_text(aes(label = Hosps), vjust = 0.5, hjust=-0.1, size=3)+
  expand_limits(x = 5000, y = 0)+
  scale_fill_gradient(low="lightblue", high = "steelblue")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title="Covid Cases by Age Group", x="", y="")+
  ggtitle("Hospitalization Count")+
  theme(legend.position = (""))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+        
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = "azure", color="azure"))

# .........................Deaths ....................................#
OhioTotal2 %>%
  filter(County %in% Counties20) %>%
  ggplot(aes(x=Deaths,y=fct_reorder(County, Cases), fill=(Deaths))) +
  geom_col() +
  geom_text(aes(label = Deaths), vjust = 0.5, hjust=-0.1, size = 3)+
  expand_limits(x = 1400, y = 0)+
  scale_fill_gradient(low="lightblue", high = "steelblue")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title="Covid Cases by Age Group", x="", y="")+
  ggtitle("Deaths Count")+
  theme(legend.position = (""))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+        
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = "azure", color="azure"))

g2_Death2<-OhioTotal2 %>%
  filter(County %in% Counties20) %>%
  ggplot(aes(x=Deaths,y=fct_reorder(County, Cases), fill=(Deaths))) +
  geom_col() +
  geom_text(aes(label = Deaths), vjust = 0.5, hjust=-0.1, size = 3)+
  expand_limits(x = 1400, y = 0)+
  scale_fill_gradient(low="lightblue", high = "steelblue")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title="Covid Cases by Age Group", x="", y="")+
  ggtitle("Deaths Count")+
  theme(legend.position = (""))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+        
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = "azure", color="azure"))

# ............................Ohio Cases on Map.......................................#
states_map <- map_data("state")
str(states_map)
head(states_map)
unique(states_map$region)
ohio_map <- subset(states_map, states_map$region=="ohio")
unique(ohio_map$region)
str(ohio_map)
head(ohio_map)
ohiocounty_map <- subset(states_map, states_map$subregion=="ohio")
unique(ohio_map$subregion)
str(ohiocounty_map)
head(ohiocounty_map)
ggplot(ohio_map, aes(x=long,y=lat,group=group))+
  geom_polygon(fill="white", colour="black")

# US Counties - from "maps" package and ggplot2::map_data   # ggplot2 function - turns maps pkg data into DF
map.county <- map_data('county')
head(map.county)                     
ohio.county <- subset(map.county, region=="ohio")
head(ohio.county)

ggplot(ohio.county, aes(x=long,y=lat,group=group,fill=subregion)) +
  geom_polygon() +
  guides(fill='none') +
  theme_map()

# ................creating another column for subregion ..........................#
OhioDF5 <- OhioCovidData2%>%
  mutate(OhioCovidData2, subregion = County)
head(OhioDF5)
BCOnsetDF4 <- OhioDF5%>% 
  group_by(subregion)%>% 
  summarise(DeathCount = sum(`DeathCount`))
head(BCOnsetDF4)

BCOnsetDF5 <- as.data.frame(BCOnsetDF4)
head(BCOnsetDF5)
BCOnsetDF5$subregion<-stringr::str_to_lower(BCOnsetDF5$subregion)
head(BCOnsetDF5)
ohio.county.map <-merge(ohio.county, BCOnsetDF5,
                        by.x="subregion", all = TRUE)
view(ohio.county.map)
ohio.county.map %>% 
  ggplot(aes(x=long,y=lat,group=group, fill=DeathCount)) +
  geom_polygon()+
  scale_fill_gradient(low="azure2", high="steelblue")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title="", x="", y="")+
  theme(legend.position = (""))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+        
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = "azure", color="azure"))

g3_Map<-ohio.county.map %>% 
  ggplot(aes(x=long,y=lat,group=group, fill=DeathCount)) +
  geom_polygon()+
  scale_fill_gradient(low="azure2", high="steelblue")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(title="", x="", y="")+
  theme(legend.position = (""))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+        
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = "azure", color="azure"))

#.......................Patchwork................................................
(g3_Map)+(g1_Case1 / g1_Hosp1 / g1_Death1)/
  (g2_Case2 + g2_Hosp2 + g2_Death2)