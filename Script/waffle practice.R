library(ggplot2)
library(waffle)

npk |> group_by(N, P) |> ggplot(aes(fill = N, values = round(yield))) + geom_waffle(n_rows = 10, flip = TRUE,radius =grid::unit(0,'npc') ) +
  facet_wrap( ~ P, strip.position = 'bottom') + coord_equal()+
  theme_minimal()+
  theme_enhance_waffle()

