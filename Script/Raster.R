library(tidyverse)
library(sf)
library(mapboxapi)
library(tigris)
library(fasterize)
library(leaflet)

# Get county boundaries; you can analyze for any county
target_GEOID <- "48309"

county_boundaries <- counties(cb = TRUE, resolution = "20m")

target_county <- filter(county_boundaries, GEOID == target_GEOID)

# Get dialysis CSV from https://rxopen.org/api/v1/map/download/facility
dialysis <- read_csv("facility.csv") %>%
  filter(Type == "Dialysis Center") %>%
  separate(CalcLocation, into = c("Y", "X"), sep = ",") %>%
  st_as_sf(coords = c("X", "Y"), crs = 4269) %>%
  st_filter(target_county)

# Calculate walk-time accessibility up to 60 minutes
# Process can take several minutes for larger counties / areas
# (rate limiting of 300 isochrones per minute)
layered_isochrones <- mb_isochrone(
  dialysis,
  time = 1:60,
  profile = "walking",
  id = "Name"
)

# Find a good CRS to use
crsuggest::suggest_crs(dialysis) %>% View()

# Make the raster template - 100m resolution
raster_template <- layered_isochrones %>%
  st_transform(6577) %>%
  raster::raster(resolution = 100)

# Calculate the accessibility raster
# Minimum walk-time to a dialysis center
iso_surface <- layered_isochrones %>%
  st_transform(6577) %>%
  fasterize(raster_template, field = "time", fun = "min")

# Map the results
pal <- colorNumeric("viridis", layered_isochrones$time, na.color = "transparent")

leaflet() %>%
  addMapboxTiles(style_url = "mapbox://styles/mapbox/streets-v11", scaling_factor = "0.5x") %>%
  addRasterImage(iso_surface, colors = pal, opacity = 0.5) %>%
  addMarkers(data = dialysis) %>%
  addLegend(values = layered_isochrones$time, pal = pal,
            title = "Walk-time to<br/>nearest dialysis center")
