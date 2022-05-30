library(tidyverse)

Plot <- iris |> select(1,2,5) |>
  group_by(Species) |> nest() |> mutate(plot=map2(.x = data,.y = Species,~ggplot(data = .x)+theme_minimal()+geom_line(aes(x = Sepal.Length,y = Sepal.Width))))


print(Plot$plot)


Plot |> pull()




Plot <- iris  |>
group_by(Species) |> nest() |> mutate(plot=map2(.x = data,.y = Species,~ggplot(data = .x)+theme_minimal()+geom_point(aes(x = Sepal.Length,y = Sepal.Width,group=1))+
labs(title = .y)))


Plot |> pull()


# Save the Plot
map2(paste0(Plot$plot, ".pdf"), Plot$plot, ggsave)
