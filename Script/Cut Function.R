# Cut Function

library(tidyverse)

iris %>% mutate(Size = cut(
  x = Sepal.Length,
  breaks = 3,
  labels = c('small', 'medium', 'large')
))
