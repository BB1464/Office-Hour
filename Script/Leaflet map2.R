
library(tidyverse)
library(sf)
library(leaflet)
library(rnaturalearth)

GM <- ne_states(country = 'Nigeria')

GM <- st_as_sf(GM)
head(GM)

leaflet(data = GM) %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addMarkers(lng =~longitude,lat = ~latitude,label = ~gn_name) %>% 
  addPolygons(stroke = TRUE,color = 'red',weight = 0.4,opacity = 0.7,fill = TRUE,fillColor = 'green',highlightOptions = highlightOptions(stroke = 1,color = 'red',weight = 1,opacity = 1,fill = 'green',fillColor = 'red',fillOpacity = 1)) %>% setView(zoom = 6,lng = 10,lat = 10)
