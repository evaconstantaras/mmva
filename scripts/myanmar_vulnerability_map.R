#install packages ggplot2 tidyverse sf readr readxl, tigris

library(ggplot2)
library(tidyverse)
library(dplyr)
library(sf)
library(readr)

#import Myanmar townships shapefile

myanmar_ts <- "data/myanmar_township_boundaries.shp"

townships<-st_read(myanmar_ts)

ggplot(townships)+
  geom_sf()

#import Myanmar vulnerability data
library(readxl)

vuln <- read_excel("data/Myanmar_MIMU_Vulnerability.xls")

glimpse(vuln)

vuln<-subset(vuln, select=-c(`State/Region Pcode`, `State/Region Name`, `District Pcode`, `District Name`, `Township Name`, `Number of Village Tracts`, `Number of Wards`))

#gather to create a column of Indicators
vuln_gather <- gather(data=vuln, key="Indicator", value = "Measure", -`Township Pcode`, na.rm=F)

#join datasets
myanmar <- left_join(vuln_gather, townships, by=c(`Township Pcode`="TS_PCODE"))

#narrow down indicators 
myanmar_dev <- myanmar %>% 
  filter(myanmar$Indicator %in% c("Avg (safe sanitation + improved drinking water)", 
                                  "Average good roof and wall",
                                  "Electricity for Lighting %__1" ,
                                  "Literacy: % literate"))


#rename indicators for clarity
myanmar_map_set <- mutate(myanmar_dev, map_labels=case_when(
                      Indicator =="Avg (safe sanitation + improved drinking water)"~"Clean water", 
                      Indicator =="Literacy: % literate"~"Literacy",
                      Indicator =="Average good roof and wall"~"Roof and walls",
                      Indicator == "Electricity for Lighting %__1"~"Electric lighting"))

head(myanmar_map_set)
                     


#map selected indicators 


ggplot(myanmar_map_set)+
  geom_sf(aes(fill=Measure*100), color="white", size=.1)+
  scale_fill_distiller(direction=1, name="Population %") +
  labs(title="Uneven development across Myanmar", caption="Source: themimu.info/")+
  facet_wrap(~map_labels)+
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent'))

ggsave("myanmar_townships.png")