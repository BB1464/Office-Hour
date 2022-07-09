
library(tidyverse)
library(ggthemes)

world_map <- map_data('world') |>
  filter(!long>180)

countries <- world_map |>
  distinct(region) |>
  rowid_to_column()


countries |>
  ggplot(aes(fill=rowid,map_id=region))+
  geom_map(map = world_map)+
  expand_limits(x=world_map$long,y=world_map$lat)+
  coord_map('merca')+
  theme_map()+scale_fill_gradient2(low = 'darkred',mid = 'white',high = 'darkred',midpoint = 0)

