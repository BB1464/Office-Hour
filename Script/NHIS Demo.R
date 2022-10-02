library(sf)
library(ggplot2)
library(viridis)

### Cancer risk prediction
nc <- st_read(system.file("shape/nc.shp", package = "sf"),
              quiet = TRUE
)

ggplot(data = nc, aes(fill = SID74)) + geom_sf() +
  scale_fill_viridis() + theme_bw()


### Rainfall Prediction
##

library(geoR)

ggplot(data.frame(cbind(parana$coords, Rainfall = parana$data)))+
  geom_point(aes(east, north, color = Rainfall), size = 2) +
  coord_fixed(ratio = 1) +
  scale_color_gradient(low = "blue", high = "orange") +
  geom_path(data = data.frame(parana$border), aes(east, north)) +
  theme_bw()

### pOINT Pattern
### 

library(cholera)

rng <- mapRange()
plot(fatalities[, c("x", "y")],
     pch = 15, col = "black",
     cex = 0.5, xlim = rng$x, ylim = rng$y, asp = 1,
     frame.plot = FALSE, axes = FALSE, xlab = "", ylab = ""
)
addRoads()



### Get Shape file
### 

# read shapefile with readOGR()

# name of the shapefile of North Carolina of the sf package
nameshp <- system.file("shape/nc.shp", package = "sf")
 nameshp

library(rgdal)
map <- readOGR(nameshp, verbose = FALSE)

class(map)

head(map@data)

### Plot the map of North Carolina
##

plot(map)

## Read in some shape file
## 

# read shapefile with st_read()
library(sf)
map <- st_read(nameshp, quiet = TRUE)

class(map)

head(map)

plot(map)


### Static Map of NC with ggplot2
### 

library(ggplot2)
map <- st_as_sf(map)
ggplot(map) + geom_sf(aes(fill = SID74)) + theme_bw()

library(viridis)
map <- st_as_sf(map)
ggplot(map) + geom_sf(aes(fill = SID74)) +
  scale_fill_viridis() + theme_bw()

png("plot.png")
ggplot(map) + geom_sf(aes(fill = SID74)) +
  scale_fill_viridis() + theme_bw()
dev.off()

### Get the crs for the map
### 

st_crs(map)

## Transform the crs
map <- st_transform(map, 4326)

### Interractive Map with Leaflet
### 

library(leaflet)

pal <- colorNumeric("YlOrRd", domain = map$SID74)

p=leaflet(map) %>%
  addTiles() %>%
  addPolygons(
    color = "white", fillColor = ~ pal(SID74),
    fillOpacity = 1
  ) %>%
  addLegend(pal = pal, values = ~SID74, opacity = 1)

### Map view demo
### 

library(mapview)
mapview(map, zcol = "SID74")


### Modify the map background
### 
library(RColorBrewer)
pal <- colorRampPalette(brewer.pal(9, "YlOrRd"))
mapview(map,
        zcol = "SID74",
        map.types = "CartoDB.DarkMatter",
        col.regions = pal
)


### Faceted map for comparism
### 

m74 <- mapview(map, zcol = "SID74")
m79 <- mapview(map, zcol = "SID79")
m <- leafsync::sync(m74, m79)
m


### Thematic map with tmap
### 

library(tmap)
tmap_mode("view")
tm_shape(map) + tm_polygons("SID74")
