
library(sf)
library(rnaturalearth)

world <- ne_countries(scale = 'large',returnclass = 'sf') |>
  st_transform(crs = 'ESRI:54019')


ggplot(data = world)+
  geom_sf(aes(fill=gdp_md_est,shape='No Data\nAvailable'))+
  scale_fill_gradient2(name='GDP\nEstimate',low = 'blue',mid = 'green',high = 'red',midpoint = 0)+
  geom_rect(aes(xmin=-8,xmax=10,ymin=-8,ymax=10),col='black',alpha=0.05,fill=NA,linewidth=1)+
  theme_light()




if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")
if(!require("lubridate")) install.packages("lubridate")
if(!require("hms")) install.packages("hms")
if(!require("terra")) install.packages("terra")
if(!require("lwgeom")) install.packages("lwgeom")
if(!require("rnaturalearth")) install.packages("rnaturalearth")
if(!require("gifski")) install.packages("gifski")



# packages
library(rnaturalearth)
library(tidyverse)
library(lwgeom)
library(sf)
library(terra)
library(lubridate)
library(hms)
library(gifski)


source("Script/terminator.R") # import the functions
t0 <- Sys.time() # date and time of our operating system
t0

coord_nightday <- terminator(t0, -180, 180, 0.2) # estimate the day-night line

# convert it into a spatial object of class sf
line_nightday <- st_linestring(as.matrix(coord_nightday)) %>% st_sfc(crs = 4326)

# plot
plot(line_nightday)



# rectangle
wld_bbx <- st_bbox(c(xmin = -180, xmax = 180,
                     ymin = -90, ymax = 90),
                   crs = 4326) %>%
  st_as_sfc()

# division with the day-night line
poly_nightday <-  st_split(wld_bbx, line_nightday) %>%
  st_collection_extract(c("POLYGON")) %>%
  st_sf()

# plot
plot(poly_nightday)



# select the second polygon
poly_nightday <- slice(poly_nightday, 2) %>%
  mutate(daynight = 1)

# create the raster with a resolution of 0.5º and the extent of the world
r <- rast(vect(wld_bbx), resolution = .5)

# rasterize the night polygon
night_rast <- rasterize(vect(poly_nightday), r)

# result in raster format
plot(night_rast)




# define the raster projection (WGS84)
crs(night_rast) <- "EPSG:4326"

# reproject
night_rast_prj <- project(night_rast, "ESRI:54009",
                          mask = TRUE,
                          method = "near")
# map
plot(night_rast_prj)




rast_determiner <- function(x_min, date, res) {

  # create date with time adding the number of minutes
  t0 <- as_date(date) + minutes(x_min)
  # estimate the coordinates of the line that separates day and night
  night_step <- terminator(t0, -180, 180, 0.2) %>% as.matrix()
  # pass the points to line
  night_line <- st_linestring(night_step) %>% st_sfc(crs = 4326)

  # define the rectangle of the planet
  wld_bbx <- st_bbox(c(xmin = -180, xmax = 180,
                       ymin = -90, ymax = 90),
                     crs = 4326) %>%
    st_as_sfc()

  # divide the polygon with the day-night line
  poly_nightday <-  st_split(wld_bbx, night_line) %>%
    st_collection_extract(c("POLYGON")) %>%
    st_sf()

  # select the polygon according to the date
  if(date <= make_date(year(date), 3, 20) | date >= make_date(year(date), 9, 23)) {

    poly_nightday <- slice(poly_nightday, 2) %>%
      mutate(daynight = 1)

  } else {

    poly_nightday <- slice(poly_nightday, 1) %>%
      mutate(daynight = 1)
  }

  # create the raster with the resolution given in the argument res
  r <- rast(vect(wld_bbx), resolution = res)

  # rasterize the night polygon
  night_rast <- rasterize(vect(poly_nightday), r)

  return(night_rast)

}




night_determinator <- function(time_seq, # minutes
                               date = Sys.Date(), # date (system default)
                               res = .5) { # raster resolution 0.5º

  # apply the first function on a vector of day intervals
  night_raster <-  map(time_seq,
                       rast_determiner,
                       date = date,
                       res = res)

  # convert the raster into an object with as many layers as day intervals
  night_raster <- rast(night_raster)

  # define the WGS84 projection
  crs(night_raster) <- "EPSG:4326"

  return(night_raster)

}




#apply our function for a 24 hour day in 30 minute intervals
night_rast <- night_determinator(seq(0, 1410, 30), Sys.Date(), res = .5)

# reproject to Winkel II
# This line is not working
night_raster_winkel <- project(night_rast,
                               "ESRI:54019",
                               mask = TRUE,
                               method = "near")
# map of the first 5 intervals
plot(night_raster_winkel, maxnl = 5)







# country boundaries
wld <- ne_countries(scale = 10, returnclass = "sf") %>%
  st_transform("ESRI:54019")

# convert the raster to a data.frame with xyz
df_winkel <- as.data.frame(night_raster_winkel, xy = TRUE, na.rm = FALSE)

# rename all the columns corresponding to the day intervals
names(df_winkel)[3:length(df_winkel)] <- str_c("H", as_hms(seq(0, 1410, 30)*60))

# change to a long format
df_winkel <- pivot_longer(df_winkel, 3:length(df_winkel), names_to = "hour", values_to = "night")

# exclude missing values to reduce table size
df_winkel <- filter(df_winkel, !is.na(night))




# graticule
grid <- st_graticule() %>%   st_transform("ESRI:54019")

# get the extension of the world
bbx <- st_bbox(wld)






# example 5 UTC
x <- "H05:00:00"
# map
ggplot() +
  # boundaries
  geom_sf(data = wld,
          fill = "#74a9cf",
          colour = "white",
          size = .1) +
  # graticule
  geom_sf(data = grid, size = .1) +
  # filtered raster data
  geom_raster(data = filter(df_winkel, hour == {{x}}),
              aes(x, y),
              fill = "grey90",
              alpha = .6) +
  # title
  labs(title = str_c("\U1F551", str_remove(x, "H"), " UTC")) +
  # extension limits
  coord_sf(xlim = bbx[c(1, 3)],
           ylim = bbx[c(2, 4)])  +
  # map style
  theme_void() +
  theme(plot.title = element_text(hjust = .1, vjust = .9))




walk(str_c("H", as_hms(seq(0, 1410, 30)*60)), function(step){

  g <- ggplot() +
    geom_sf(data = wld,
            fill = "#74a9cf",
            colour = "white",
            size = .1) +
    geom_sf(data = grid,
            size = .1) +
    geom_raster(data = filter(df_winkel, hour == {{step}}), aes(x, y),
                fill = "grey90",
                alpha = .6) +
    labs(title = str_c("\U1F551", str_remove(x, "H"), " UTC")) +
    coord_sf(xlim = bbx[c(1, 3)], ylim = bbx[c(2, 4)])  +
    theme_void() +
    theme(plot.title = element_text(hjust = .1, vjust = .9))


  ggsave(str_c("wld_night_", str_remove_all(step, ":"), ".png"), g,
         height = 4.3, width = 8.4, bg = "white", dpi = 300, units = "in")

})




# Save the images into a file
files <- str_c("wld_night_H", str_remove_all(as_hms(seq(0, 1410, 30)*60), ":"), ".png")


# Convert the Images into a annimated giff
gifski(files, "night_day.gif", width = 807, height = 409, loop = TRUE, delay = 0.1)
