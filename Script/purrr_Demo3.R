
library(tidyverse)
library(furrr)


iris |>
  nest(data=-Species) |>
  mutate(model=map(.x = data,.f =function(x){ggplot(data = x,aes(x = Sepal.Length,y = Sepal.Width))+geom_point()}))


mtcars |> map(.f = function(x) mean(x))

lapply(X = mtcars,function(x)length(unique(x)))


map(mtcars,function(x)unique(x))

map(.x = mtcars,.f = ~unique(.x))

map(.x = mtcars,.f = unique)

start <- Sys.time()

iris |>
  nest(data=-Species) |>
  mutate(model=future_map(.x = data,.f = ~lm(Sepal.Length~Sepal.Width,data=.x))) |>
 # mutate(anova=map(.x = model,.f = ~anova(.x) |> broom::tidy(.x))) |>
  mutate(tid=future_map(.x = model,.f = ~broom::augment(.x))) |>
  mutate(summary=future_map(.x = model,.f = ~summary(.x))) |>
  walk(print)

end <- Sys.time()

end-start


x <- 10
g01 <- function() {
  x <- 20
  x
}
g01(x = 40)




iris |>
  nest(data = -Species) |>
  transmute(plot=map(.x = data,.f = ~ggplot(data = .x,aes(x = Sepal.Length,y = Petal.Length))+geom_smooth(se = TRUE)+geom_point(position = position_jitter(.2)))) |>
  walk(print)
