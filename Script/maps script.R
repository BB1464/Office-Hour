library(tidyverse)
library(readxl)
library(leaflet)

dat2=read_excel('Data/maps.xlsx')



leaflet() %>%
addTiles() %>%
addMarkers(lng = dat2$Longitude,lat = dat2$Latitude,data = dat2,label =~`S/N`)
#addPolygons(lng = dat2$Longitude,lat = dat2$Latitude,data = dat2) %>%
#addCircleMarkers(lng = dat2$Longitude,lat = dat2$Latitude,data = dat2)





dat3 <- map_data('world')



library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)


world <- ne_countries(scale = 'medium',returnclass = 'sf')
NG <- world %>% filter(sovereignt=='Nigeria')



library(rnaturalearth)

dat <- ne_states(country = 'Nigeria',returnclass = 'sf')


ggplot(data = dat,mapping = aes(x = longitude,y = latitude))+geom_sf()+geom_text(aes(label=gn_name))+



# String replacement
dat$gn_name <- str_replace(string = dat$gn_name,pattern = 'State',replacement = '')

leaflet() %>%
  addTiles() %>%
  addMarkers(lng = dat$longitude,lat = dat$latitude,data = dat,label =~gn_name)
