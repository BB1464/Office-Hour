library(ggspatial)
library(mdsr)
library(sf)

ggplot(CholeraDeaths) + 
  annotation_map_tile(type = "osm", zoomin = 0) + 
  geom_sf(aes(size = Count), alpha = 0.7)


library(mapproj)
library(maps)
map("world", projection = "mercator", wrap = TRUE)

map("world", projection = "cylequalarea", param = 45, wrap = TRUE)



## Leaflet demo

white_house <- tibble(
  address = "The White House, Washington, DC"
) %>%
  tidygeocoder::geocode(address, method = "osm")

library(leaflet)

white_house_map <- leaflet() %>% 
  addTiles() %>%
  addMarkers(data = white_house)

white_house_map


