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


# Model
mtcars_nested <- mtcars %>%
  group_by(cyl) %>%
  nest()


mtcars_nested

# Fit the Model
mtcars_nested <- mtcars_nested %>%
  mutate(model = map(data, function(df) ~lm(mpg ~ wt, data = df)))

mtcars_nested



# List of Prediction

mtcars_nested <- mtcars_nested %>%
  mutate(model = map(model, predict))

mtcars_nested



library(purrr)

mtcars %>%
  split(.$cyl) %>% # from base R
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

# purr model function

mtcars_model <- mtcars |> group_by(cyl) |>
  nest() |>
  mutate(model=map(.x = data,.f = ~lm(formula = mpg~vs+am,data = .))) |>
mutate(model_summary=map(.x = model,.f = ~summary(.))) |>
  mutate(anova=map(.x = model,.f = ~anova(.))) |>
  mutate(rsqr=map_dbl(.x = model_summary,.f = ~.[['r.squared']]))


# View the output from the model
mtcars_model$anova[[1]]
mtcars_model$model_summary[[1]]

mtcars_model$model_summary[[1]][['r.squared']]


mtcars_model |> pluck('model_summary',1)

mtcars_model |> pluck('anova',1)


mtcars_model |> pluck('anova')

mtcars_model |> pluck('model_summary')

mtcars_model |> pluck('model')

mtcars_model |> chuck('anova')


# Shot cut for anova and summary
summary(mtcars_model$model[[1]]) # for Anova

anova(mtcars_model$model[[1]]) # Return the summary




