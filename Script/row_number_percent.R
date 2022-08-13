
data(mpg)
head(mpg)
library(MetBrewer)

mpg |>
group_by(manufacturer) |>
summarise(count=n()) |>
  arrange(desc(count)) |>
mutate(perct=scales::percent(x = count/sum(count),accuracy = .1,trim = FALSE),
       perct=if_else(row_number()==1,paste(perct,'of all car models'),perct))  |>
  ggplot(aes(x = fct_reorder(.f = manufacturer,.x = perct,.desc = FALSE),y = count))+
  geom_col(show.legend = FALSE)+
    geom_text(aes(label=perct),hjust=1,size=5,fontface='bold',)+
  scale_y_continuous(expand = c(0,0),limits = c(0,40))+
  scale_x_discrete(expand = c(0,0))+
  coord_flip(clip = 'off')



mpg |>
  group_by(manufacturer) |>
  summarise(count = n()) |>
  mutate(perct = scales::percent(
    x = count / sum(count),
    accuracy = .1,
    trim = FALSE
  ))  |>
  mutate(perct = if_else(condition = row_number() == 3, true = paste(perct, 'of all car models'), false = perct)) |>
  ggplot(aes(
    x = fct_reorder(.f = manufacturer, .x = perct, .desc = FALSE),
    y = count
  )) +
  geom_col() +
  geom_text(
    aes(label = perct),
    hjust = 1,
    size = 5,
    fontface = 'bold',
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_flip(clip = 'off')
