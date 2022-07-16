library(ggplot2)
library(ggspatial)
library(sf)
library(rnaturalearth)

world <- rnaturalearth::ne_countries(scale = "medium",
                                     returnclass = "sf")
# Demo data frame for shading
a <- st_as_sf(data.frame(plot_id = 1, lat = -100, long = 36),
              coords = c("lat", "long"), crs = 4326)

# map with shading of study location
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = a, shape = 0, size = 35, color = "red", stroke = 2) +
  coord_sf(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")



ggplot() +
  geom_sf(data = world) +
  geom_rect(aes(xmin = -132, xmax = -69, ymin = 23, ymax = 49), color = "red", fill = NA)  +
  coord_sf(crs = "+proj=lonlat +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")



# Annotation will also work
ggplot(data = mtcars,aes(x = mpg,y = cyl))+
  geom_jitter()+
  geom_rect(aes(xmin=15,xmax=20,ymin=5,ymax=6),col='red',linewidth=1,fill=NA)

#annotate('rect',xmin=15,xmax=20,ymin=5,ymax=6,col='red',linewidth=1,fill=NA)



ggplot() +
  geom_sf(data = world) +
  geom_rect(aes(xmin = -2464417, xmax = 2574417, ymin = -3064417, ymax = 1264417), color = "red", fill = NA)  +
  coord_sf(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")




# Interraction Plot

mtcars %>%
  ggplot(aes(factor(cyl), mpg, group = interaction(am, vs),
             color = interaction(am, vs))) +
  stat_summary(geom = 'line', fun = mean) +
  stat_summary(geom = 'point', fun = mean, color = 'black') +
  stat_summary(geom = 'errorbar', width = 0.1, alpha = 0.8, size = 0.3,fun.data = mean_se) +
  labs(color = 'Conditions',x='cyl') +
  scale_color_brewer(palette = 'Set2') +
  theme_test(base_size = 16)
