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


p=iris |>
  nest(data = -Species) |>
  mutate(chart=map(.x = data,.f = function(x){
    ggplot(data = x,aes(x = Sepal.Length,y = Petal.Width))+
      geom_point()+
      stat_smooth()
  })) |>
  select(Species,chart) |>
  walk(print)

# Save the Plot
map2(paste(p$Species,'.png'),p$chart,.f = ggsave,path=here::here('Plot'))


# Add Alternative Function

iris |>
  nest(data=-Species) |>
  mutate(plot=map2(.x = data,.y = Species,.f = ~ggplot(data = .x,aes(x = Sepal.Length,y = Petal.Length))+geom_point(position = position_jitter())+
                     geom_smooth()+
                     theme_classic()+
                     scale_color_viridis_d()+
                     labs(title = .y)
  )) |>
  select(Species,plot) |>
  walk(print)


iris |>
  nest(data=-Species) |>
  mutate(plot=map2(.x = data,.y = Species,.f = function(x,y){
    ggplot(data = x,aes(x = Sepal.Length,y = Petal.Width))+
      geom_point(position = position_jitter())+
      geom_smooth()+
      labs(title = y)
  })) |>
  select(Species,plot) |>
  pull('plot')
