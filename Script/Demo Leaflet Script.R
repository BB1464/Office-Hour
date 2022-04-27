
library(tidyverse)
library(readxl)
library(rnaturalearth)
library(leaflet)
library(leaflet.extras)


# Farms
dat2=read_excel('Data/maps.xlsx',sheet = 'Farms')


# Leaflet Map

leaflet(data = dat2) %>%
addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
addMarkers(lng = ~Longitude,lat = ~Latitude,label =~LGA)



addCircleMarkers(lng = ~Longitude,lat = ~Latitude,label=~LGA,fill = TRUE,fillColor = ~LGA)




