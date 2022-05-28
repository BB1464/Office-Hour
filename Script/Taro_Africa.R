library(sf)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)

library(afrilearndata)

data("africountries")

africountries |>
  mutate()

dat <- dat |> mutate(name=fct_recode(name,'IV'="Côte d'Ivoire"))

dat <- dat |>
  mutate(Reg=name%in%c('Burundi','Central African Rep.','Benin','IV','Cameroon','Congo','Dem. Rep. Congo','Egypt','Ethiopia','Gabon','Ghana','Guinea','Madagascar','Liberia','Nigeria','Rwanda','Sierra Leone','Togo','Chad'))


ggplot(data = dat,aes(fill=Reg))+geom_sf(show.legend = FALSE)+
  scale_fill_manual(values = c('white','blue'))+
  theme_void()


