# install.packages(c("osmdata", "ggplot2", "ggmap"))
library(osmdata)
library(ggplot2)
library(ggmap)

# creating bounding box for Lagos
lagos_bb <- getbb("Lagos")

# retrieving data of hospitals in Lagos
lagos_hospitals <- lagos_bb %>%
  opq() %>%
  add_osm_feature(key = "amenity", value = "hospital") %>%
  osmdata_sf()

# retrieving map of lagos
lagos_map <- get_map(lagos_bb, maptype = "roadmap")

# visualising map of lagos overlayed by hospitals in lagos
ggmap(lagos_map) +
  geom_sf(
    data = lagos_hospitals$osm_polygons,
    inherit.aes = FALSE,
    colour = "#08519c",
    fill = "#08306b",
    alpha = .5,
    size = 1
  ) +
  labs(
    title = "Hospitals in Lagos(Nigeria)",
    x = "Latitude",
    y = "Longitude"
  )


# Second Approach

library(osmdata)
library(ggplot2)

# creating bounding box for Lagos
lagos_bb <- getbb("Lagos")

# retrieving data of streets in Lagos
lagos_streets <- lagos_bb %>%
  opq() %>%
  add_osm_feature("highway", c("motorway", "primary", "secondary", "tertiary")) %>%
  osmdata_sf()

# retrieving data of small streets in Lagos
lagos_small_streets <- lagos_bb %>%
  opq() %>%
  add_osm_feature(key = "highway", value = c("residential", "living_street", "unclassified", "service", "footway")) %>%
  osmdata_sf()

# retrieving data of rivers in Lagos
lagos_rivers <- lagos_bb %>%
  opq() %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

# retrieving data of hospitals in Lagos
lagos_hospitals <- lagos_bb %>%
  opq() %>%
  add_osm_feature("amenity", "hospital") %>%
  osmdata_sf()

