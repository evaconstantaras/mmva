#install packages ggplot2 tidyverse, dplyr, sf, readr, readxl, tigris

library(ggplot2)
library(tidyverse)
library(dplyr)
library(sf)
library(readr)
library(DT)


#bring in state names 

myanmar_chart_2 <- vuln %>% 
                  left_join(townships, by=c(`Township Pcode`="TS_PCODE"))

#calculate total population and total vulnerable population by state

myanmar_sum <- myanmar_chart_2 %>% 
  group_by(ST) %>% 
  summarize(total_pop=sum(`Total Pop Both sexes`), total_vp=sum(`Approximate Vulnerable Population`)) 
  datatable(myanmar_sum)
  

  
#Calculate percentage of population that is vulnerable
  
myanmar_sum <- mutate(myanmar_sum, perc_vuln_pop=total_vp/total_pop*100) %>% 
  arrange(desc(perc_vuln_pop))
datatable(myanmar_sum)

ggplot(data = subset (myanmar_sum, ST %in% ST [1 : 9]))+ 
       aes(x=reorder(ST, perc_vuln_pop), y=perc_vuln_pop, fill=ST)+
  geom_bar(stat="identity")+
  coord_flip()+
  xlab("States") + ylab("Vulnerable population (%)")+
  labs(title="Ethnic states most vulnerable before current crisis", 
       subtitle = "Over half of citizens vulnerable in the poorest and most marginalized ethnic states",
       caption="Source: Vulnerability Assessement, 2014 Census, themimu.info/")+
      scale_fill_brewer()+
      theme(legend.position="none")
    
ggsave("myanmar_states.png")
  
  