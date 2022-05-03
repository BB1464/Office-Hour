library(tidyverse)
library(sf)
library(rnaturalearthdata)

states50 %>%
  st_as_sf() %>%
  filter(admin=='United States of America') %>%
  filter(name!='Alaska' & name!='Hawaii') %>%
  ggplot()+
  geom_sf(size=0,col=NA,fill='red')+
  theme_void()
