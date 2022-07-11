
mean_fun <- function(group_var,sum_mean){
  mtcars |> group_by({{group_var}}) |>
    summarise(Mean=mean({{sum_mean}}))
}

mean_fun(group_var = vs,sum_mean = mpg)


plot_fun <- function(x,y,group,labs,...){
  iris |>
    ggplot(aes(x = {{x}},y = {{y}},col={{group}}))+
    geom_point()
}


plot_fun(x = Sepal.Length,y = Sepal.Width,group = Species,labs(x='Sepal Length'))


demo <- function(...){
iris |>
    filter(...) |>
    select(...)
}

demo(Speices=='setosa')





pp=ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class),size=5) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(
      nrow = 1,
      override.aes = list(size = 5)
    )
  )
#> `geom_smooth()` using method = 'loess'


head(penguins)

penguins |>
  filter(if_any(.cols = everything(),.fns = ~!is.na(.x))))


penguins |>
  filter(if_all(.cols = everything(),.fns = ~!is.na(.x))) |>
  ggplot(aes(x = species,y = bill_length_mm,col=sex))+
  geom_point(position = position_jitter(.2),alpha=0.5)+
  stat_summary(geom = 'point',fun = 'mean',shape='diamond',size=3,col='black')+
  stat_summary(geom = 'errorbar',fun.data = mean_se,aes(group=1),width=.3)+
  facet_grid(~year)+
  scale_color_viridis_d()+
  theme_test()+
  theme(strip.background = element_rect(fill = NA))
