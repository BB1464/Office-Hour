
library(gapminder)

data("gapminder")
head(gapminder)


Plot <- gapminder |>
  group_by(country) |>
  nest() |>
  mutate(plot=map2(.x = data,.y = country,.f = ~ggplot(data = .x,aes(x = year,y = pop))+geom_line()+coord_cartesian(expand = FALSE)+labs(title =.y)))


Plot |> pluck('plot')


#map2(paste(x=Plot$country,'.png'),.y = Plot$plot,.f = ggsave)
