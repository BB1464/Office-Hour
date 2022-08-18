
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

# Import the Agromophological maps data

agro <- read_excel(path = here::here('Data/Agromorphological map coordinates.xlsx'),sheet = 2)

#col <- read_excel(path = here::here('Data/Agro-Joy.xlsx'))

# Converrt it to sf object

#col1 <- col |> st_as_sf(coords=c('longitude','latitude'),crs=4326)


# Farms
dat2=read_excel('Data/maps.xlsx',sheet ='Farms')



##################################################################
##                        Data Wrangling                        ##
##################################################################


dat <- ne_states(country = 'Nigeria',returnclass = 'sf')



# String replacement
dat$gn_name <- str_replace(string = dat$gn_name,pattern = 'State',replacement = '')


dat$gn_name <- str_replace(string =dat$gn_name,pattern = 'Federal Capital Territory',replacement = 'FCT')



# data Wrangling for the Agro-Ecological Zones

dat <- dat |> mutate(across(.cols = 'name',.fns = as.character))


# dat |>
#   mutate(name=case_when(str_detect(name%in%c('Kebbi','Kano','Sokoto','Zamfara','Katsina','Jigawa','Katsina','Borno')~'Humid Savannah'),
#                         str_detect(name %in%c('Kaduna','Bauchi','Gombe')~'Northern Guinea Savanna'),str_detect(name %in%c('Oyo','Ogun','Osun','Ekiti','Nassarawa','Kwara','Federal Capital Territory','Benue','Taraba','Enugu','Ebonyi','Cross River','Kogi')~'Derived Savanna'),
#                         str_detect(name %in%c('Ondo','Lagos','Edo','Delta','Bayelsa','Rivers','Abia','Imo','Anambra','Akwa Ibom')~'Humid Forest'),
#                         str_detect(name %in%c('Niger','Adamawa'))~'Southern Guinea Savanna',TRUE~'MidAltitude'))
#



dat <- dat |>
  mutate(name2=name) |>
  mutate(name=if_else(condition = name=='Kebbi',true = 'Humid Savannah',false = name)) |>
  mutate(name=if_else(condition = name=='Kano',true = 'Humid Savannah',false = name)) |>
  mutate(name=if_else(condition = name=='Sokoto',true = 'Humid Savannah',false = name)) |>
  mutate(name=if_else(condition = name=='Zamfara',true = 'Humid Savannah',false = name)) |>
  mutate(name=if_else(condition = name=='Katsina',true = 'Humid Savannah',false = name)) |>
  mutate(name=if_else(condition = name=='Jigawa',true = 'Humid Savannah',false = name)) |>
  mutate(name=if_else(condition = name=='Yobe',true = 'Humid Savannah',false = name)) |>
  mutate(name=if_else(condition = name=='Borno',true = 'Humid Savannah',false = name)) |>
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











# Convert the excel file to sf object
#dat2 <- dat2 |>
#st_as_sf(coords=c('Longitude','Latitude'),crs=4326)





# Visualization

ggplot(data = dat,mapping = aes(x = longitude,y = latitude))+geom_sf()+geom_text(aes(label=gn_name))+
  geom_point(data = dat2,mapping = aes(x = Longitude,Latitude),size=3)+
  theme_void()

# Updated Script

ggplot(data = dat,mapping = aes(x = longitude,y = latitude))+geom_sf(show.legend = FALSE)+
  geom_sf(data = dat,aes(fill=name))+
  geom_text(data = dat,aes(label=gn_name))+
  geom_point(data=AG,aes(x = Longitude,y = Latitude,shape='19'),size=3)+theme_void()+labs(fill='Agroecological Zones')+
scale_shape_manual(label='Sampling sites',breaks = 19,values = 19,guide=guide_legend(direction = 'vertical',title = ''))+
scale_fill_met_d(name = 'Lakota',override.order = TRUE)+
  theme(legend.title = element_text(family = 'serif',face = 'bold',size = 14))


# Export the map

ggsave(path = 'Plot','Farms.png',width = 12,height = 8,dpi = 400)






# Markets

dat3=read_excel('Data/maps.xlsx',sheet = 'Markets')


##################################################################
##                        Data Wrangling                        ##
##################################################################


dat <- ne_states(country = 'Nigeria',returnclass = 'sf')



# String replacement
dat$gn_name <- str_replace(string = dat$gn_name,pattern = 'State',replacement = '')


dat$gn_name <- str_replace(string =dat$gn_name,pattern = 'Federal Capital Territory',replacement = 'FCT')

dat3$Latitude <- as.numeric(dat3$Latitude)


# Visualization


ggplot(data = dat,mapping = aes(x = longitude,y = latitude))+geom_sf()+geom_text(aes(label=gn_name))+
  geom_point(data = dat3,mapping = aes(x = Longitude,Latitude),size=3)+
  theme_void()


# Updated Script
ggplot(data = dat,mapping = aes(x = longitude,y = latitude))+geom_sf()+geom_text(aes(label=gn_name))+
  geom_point(data = dat3,mapping = aes(x = Longitude,Latitude,col=State),size=3)+
  theme_void()+scale_color_brewer(palette = 'Dark2',direction = -1)


# Export the map

ggsave(path = 'Plot','Market.png',width = 12,height = 8,dpi = 400)


# Survey locs

dat4=read_excel('Data/maps.xlsx',sheet = 3)


##################################################################
##                        Data Wrangling                        ##
##################################################################


dat <- ne_states(country = 'Nigeria',returnclass = 'sf')



# String replacement
dat$gn_name <- str_replace(string = dat$gn_name,pattern = 'State',replacement = '')


dat$gn_name <- str_replace(string =dat$gn_name,pattern = 'Federal Capital Territory',replacement = 'FCT')

#dat4$Latitude <- as.numeric(dat4$Latitude)


# Visualization


ggplot(data = dat,mapping = aes(x = longitude,y = latitude))+geom_sf()+geom_text(aes(label=gn_name))+
  geom_point(data = dat4,mapping = aes(x = Longitude,Latitude),size=3)+
  theme_void()


# Updated Script

ggplot(data = dat,mapping = aes(x = longitude,y = latitude))+geom_sf()+geom_text(aes(label=gn_name))+
  geom_point(data = dat4,mapping = aes(x = Longitude,Latitude,col=State),size=3)+
  theme_void()+scale_color_brewer(palette = 'Dark2',direction = -1)



# Export the map

ggsave(path = 'Plot','Survey.png',width = 12,height = 8,dpi = 400)



