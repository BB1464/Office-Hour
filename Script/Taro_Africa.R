library(sf)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)

library(afrilearndata)

data("africountries")


dat <- africountries |> mutate(name=fct_recode(name,'IV'="Côte d'Ivoire"))

dat <- dat |>
  mutate(Reg=name%in%c('Burundi','Central African Rep.','Benin','IV','Cameroon','Congo','Dem. Rep. Congo','Egypt','Ethiopia','Gabon','Ghana','Guinea','Madagascar','Liberia','Nigeria','Rwanda','Sierra Leone','Togo','Chad'))


ggplot(data = dat,aes(fill=Reg))+geom_sf(show.legend = FALSE)+
  scale_fill_manual(values = c('white','blue'))+
  theme_void()


# Save the Plot

ggsave('Africa_Taro.png',width = 10,height = 8,dpi = 500,path = here::here('Plot'))



# Second Approach

library(ggplot2)
library(ggrepel)

ggplot(dat) +
  geom_sf(aes(fill=Reg),show.legend = FALSE) +
  theme_void() +
  geom_text_repel(aes(label=name_long, geometry=geometry),
                  stat="sf_coordinates",
                  point.padding = NA, #allows points to overlap centroid
                  colour='black', size=3
  ) +
  scale_fill_manual(values = c('white','blue'))


# Save the Plot
ggsave('Africa_Taro2.png',width = 10,height = 8,dpi = 500,path = here::here('Plot'))


# Afrimap Tutorial
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

