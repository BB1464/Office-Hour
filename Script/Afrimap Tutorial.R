library(mapview)
library(tmap)
library(sf)
library(raster)
library(afrilearndata)



tmap_mode('view') # For interractive mode
tmap_mode('plot') # For static mode

tmap::tm_shape(afripop2020) +
  tm_raster(palette = rev(viridisLite::magma(5)), breaks=c(0,2,20,200,2000,25000)) +
  tm_shape(africountries) +
  tm_borders("white", lwd = .5) +
  #tm_text("iso_a3", size = "AREA") +
  tm_shape(afrihighway) +
  tm_lines(col = "red") +
  tm_shape(africapitals) +
  tm_symbols(col = "blue", alpha=0.4, scale = .6 ) +
  tm_legend(show = FALSE)



library(ggplot2)
library(ggrepel)

ggplot(africountries) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c() +
  theme_void() +
  geom_text_repel(aes(label=name_long, geometry=geometry),
                  stat="sf_coordinates",
                  point.padding = NA, #allows points to overlap centroid
                  colour='darkgrey', size=3
  ) +
  labs(title = "Population by country 2000", fill = "Population Estimate")





library(mapview)

mapview(afripop2020,
        at = c(0,2,20,200,2000,25000))
