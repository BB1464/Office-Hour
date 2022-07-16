############################################################################
############################################################################
###                                                                      ###
###                              PURRR DEMO                              ###
###                                                                      ###
############################################################################
############################################################################



library(tidyverse)
library(broom)

iris |>
  nest(data=-Species) |>
  mutate(model=map(.x = data,.f =~lm(formula = Sepal.Length~Petal.Width,data=.x))) |> mutate(Tid=map(.x = model,.f = tidy)) |>
  select(Species,Tid) |>
  unnest(cols = 'Tid') |>
  ggplot(aes(x = term,y = estimate,col=Species))+
  stat_summary(geom = 'point',fun = 'mean')


iris |>
  nest(data=-Species) |>
  mutate(plot=map(.x = data,.f = function(x){
    ggplot(data = x,aes(x = Sepal.Length,y = Sepal.Width))+
      geom_point()
  }))


iris |>
  nest(data=-Species) |>
  mutate(mod=map(.x = data,.f = function(x){
    lm(formula = Petal.Length~Petal.Width,data=x)
  })) |>
  mutate(mo=map(.x = mod,.f = tidy)) |>
  select(Species,mo) |>
  walk(print)




iris |>
  nest(data = -Species) |>
  mutate(mod = map(
    .x = data,
    .f = function(c) {
      lm(formula = Sepal.Length ~ Petal.Length, data = c)
    }
  )) |>
  mutate(tidy = map(.x = mod, .f = tidy)) |>
  select(Species, tidy) |>
  unnest(tidy)


iris |>
  nest(data = -Species) |>
  mutate(chart=map(.x = data,.f = function(x){
    ggplot(data = x,aes(x = Sepal.Length,y = Petal.Width))+
      geom_point()+
      stat_smooth()
  })) |>
  select(Species,chart) |>
  walk(print)
