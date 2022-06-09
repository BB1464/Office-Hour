

library(tidyverse)
library(sf)
library(afrilearndata)
library(afriadmin)
library(rnaturalearthdata)
library(rnaturalearth)
library(readxl)
library(ggrepel)


dat <- read_excel('Data/benin_natasha.xlsx')


Benin <- ne_states(country = 'Benin')

Benin <- st_as_sf(Benin)

Palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#000000","#006400","#A2B5CD","#D15FEE","#787878","#FFFF00","#A2B5CD","#8B2323","#8B3E2F")


# Map of Benin
ggplot(data = Benin,aes(x = longitude,y = latitude))+geom_sf()+
  geom_point(data = dat,aes(x = Longitude,y = latitude,col=Village),size=4)+
 scale_colour_manual(values = Palette)+
  #  scale_colour_manual(values = c("#006400","#8B2323","#00008B","#8B3E2F",'#567899',"#FF1493","#787878","#00EE00","#FF4500","#030303","#00EEEE","#8470FF","#CD0000","#CD0000","#FFFF00","#A2B5CD","#D15FEE"))+
  geom_text_repel(data = Benin,aes(label=woe_name))+
  labs(col = 'Sampling Site')+
  theme_void()
