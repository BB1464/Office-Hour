
###########################################################################
###########################################################################
###                                                                     ###
###                          MAP OF STUDY AREA                          ###
###                                                                     ###
###########################################################################
###########################################################################



library(tidyverse)
library(readxl)
library(rnaturalearth)
library(sf)
library(MetBrewer)
library(ggspatial)
library(tidygeocoder)


# Import the Agromophological maps data



location <- tibble(long=c(3.899535,3.694969),lat=c(7.496469,6.852028),location=c('Ibadan','Ikenne'))


## Get the names of all the Address with reverse_geocoder function

rev1 <- location %>%
  reverse_geocode(lat = lat, long = long, address = addr, method = "osm")

rev1

##################################################################
##                        Data Wrangling                        ##
##################################################################


dat <- ne_states(country = 'Nigeria',returnclass = 'sf')



# String replacement
dat$gn_name <- str_replace(string = dat$gn_name,pattern = 'State',replacement = '')


dat$gn_name <- str_replace(string =dat$gn_name,pattern = 'Federal Capital Territory',replacement = 'FCT')



# data Wrangling for the Agro-Ecological Zones

dat <- dat |> mutate(across(.cols = 'name',.fns = as.character))



dat <- dat |>
  mutate(name2=name) |>
  mutate(name=if_else(condition = name=='Kebbi',true = 'Sudan Savannah',false = name)) |>
  mutate(name=if_else(condition = name=='Kano',true = 'Sudan Savannah',false = name)) |>
  mutate(name=if_else(condition = name=='Sokoto',true = 'Sudan Savannah',false = name)) |>
  mutate(name=if_else(condition = name=='Zamfara',true = 'Sudan Savannah',false = name)) |>
  mutate(name=if_else(condition = name=='Katsina',true = 'Sudan Savannah',false = name)) |>
  mutate(name=if_else(condition = name=='Jigawa',true = 'Sudan Savannah',false = name)) |>
  mutate(name=if_else(condition = name=='Yobe',true = 'Sahel Savannah',false = name)) |>
  mutate(name=if_else(condition = name=='Borno',true = 'Sahel Savannah',false = name)) |>
  mutate(name=if_else(condition = name=='Kaduna',true = 'Northern Guinea Savanna',false = name)) |>
  mutate(name=if_else(condition = name=='Bauchi',true = 'Northern Guinea Savanna',false = name)) |>
  mutate(name=if_else(condition = name=='Gombe',true = 'Northern Guinea Savanna',false = name)) |>
  mutate(name=if_else(condition = name=='Oyo',true = 'Derived Savanna',false = name)) |>
  mutate(name=if_else(condition = name=='Ogun',true = 'Derived Savanna',false = name)) |>
  mutate(name=if_else(condition = name=='Osun',true = 'Derived Savanna',false = name)) |>
  mutate(name=if_else(condition = name=='Ekiti',true = 'Derived Savanna',false = name)) |>
  mutate(name=if_else(condition = name=='Kwara',true = 'Derived Savanna',false = name)) |>
  mutate(name=if_else(condition = name=='Nassarawa',true = 'Derived Savanna',false = name)) |>
  mutate(name=if_else(condition = name=='Federal Capital Territory',true = 'Derived Savanna',false = name)) |>
  mutate(name=if_else(condition = name=='Benue',true = 'Derived Savanna',false = name)) |>
  mutate(name=if_else(condition = name=='Taraba',true = 'Derived Savanna',false = name)) |>
  mutate(name=if_else(condition = name=='Enugu',true = 'Derived Savanna',false = name)) |>
  mutate(name=if_else(condition = name=='Ebonyi',true = 'Humid Forest',false = name)) |>
  mutate(name=if_else(condition = name=='Cross River',true = 'Derived Savanna',false = name)) |>
  mutate(name=if_else(condition = name=='Kogi',true = 'Derived Savanna',false = name)) |>
  mutate(name=if_else(condition = name=='Ondo',true = 'Humid Forest',false = name)) |>
  mutate(name=if_else(condition = name=='Lagos',true = 'Humid Forest',false = name)) |>
  mutate(name=if_else(condition = name=='Edo',true = 'Humid Forest',false = name)) |>
  mutate(name=if_else(condition = name=='Delta',true = 'Humid Forest',false = name)) |>
  mutate(name=if_else(condition = name=='Bayelsa',true = 'Humid Forest',false = name)) |>
  mutate(name=if_else(condition = name=='Rivers',true = 'Humid Forest',false = name)) |>
  mutate(name=if_else(condition = name=='Abia',true = 'Humid Forest',false = name)) |>
  mutate(name=if_else(condition = name=='Imo',true = 'Humid Forest',false = name)) |>
  mutate(name=if_else(condition = name=='Anambra',true = 'Humid Forest',false = name)) |>
  mutate(name=if_else(condition = name=='Akwa Ibom',true = 'Humid Forest',false = name)) |>
  mutate(name=if_else(condition = name=='Plateau',true = 'MidAltitude',false = name)) |>
  mutate(name=if_else(condition = name=='Niger',true = 'Southern Guinea Savanna',false = name)) |>
  mutate(name=if_else(condition = name=='Adamawa',true = 'Southern Guinea Savanna',false = name))

# Convert the name to factor

dat$name <- as.factor(dat$name)


# Updated Script


ggplot(data = dat,mapping = aes(x = longitude,y = latitude))+geom_sf(show.legend = FALSE)+
  geom_sf(data = dat,aes(fill=name))+
  geom_text(data = dat,aes(label=gn_name))+
  geom_point(data=location,aes(x = long,y = lat,shape='19'),size=3,col='#981234')+theme_void()+labs(fill='Agroecological Zones')+
  scale_shape_manual(label='Sampling sites',breaks = 19,values = 19,guide=guide_legend(direction = 'vertical',title = ''))+
  scale_fill_met_d(name = 'Lakota')+
  theme(legend.title = element_text(family = 'serif',face = 'bold',size = 14),text = element_text(family = 'serif',size = 20))+
  annotation_scale(location='bl',bar_cols = c("grey60", "white"),    text_family = "serif")+
  annotation_north_arrow(location = "tl", which_north = "true",pad_x = unit(0.4, "in"), pad_y = unit(0.2, "in"),style =north_arrow_nautical(fill = c("grey40", "white"),line_col = "grey20",text_family = "serif"))




# Export the map

ggsave(path = 'Plot','Farms.png',width = 14,height = 8,dpi = 320,bg = 'white')






