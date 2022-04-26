
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

# Farms
dat2=read_excel('Data/maps.xlsx',sheet = 'Farms')



##################################################################
##                        Data Wrangling                        ##
##################################################################


dat <- ne_states(country = 'Nigeria',returnclass = 'sf')



# String replacement
dat$gn_name <- str_replace(string = dat$gn_name,pattern = 'State',replacement = '')


dat$gn_name <- str_replace(string =dat$gn_name,pattern = 'Federal Capital Territory',replacement = 'FCT')



# Visualization
#
ggplot(data = dat,mapping = aes(x = longitude,y = latitude))+geom_sf()+geom_text(aes(label=gn_name))+
  geom_point(data = dat2,mapping = aes(x = Longitude,Latitude),size=3)+
  theme_void()


# Updated Script
ggplot(data = dat,mapping = aes(x = longitude,y = latitude))+geom_sf()+geom_text(aes(label=gn_name))+
  geom_point(data = dat2,mapping = aes(x = Longitude,Latitude,col=State),size=3)+
  theme_void()+scale_color_brewer(palette = 'Dark2',direction = -1)



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



