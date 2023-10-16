
library(tidyverse)


help.search(pattern = "^geom_",package = "ggplot2") %>%
  pluck("matches") %>%
  select(1:2) %>%
  distinct()

help.search(apropos = "^geom_",package = 'ggplot2') %>%
  pluck("matches") %>%
  select(1:2) %>%
  distinct()


help.search(apropos = "^pivot_",package = 'tidyr') %>%
  pluck("matches") %>%
  select(1:2) %>%
  distinct()
