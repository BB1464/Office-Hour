library(tidyverse)
library(agricolae)
library(broom)


dt <- iris |> pivot_longer(-Species) |>
  group_by(name) |>
  nest() |>
  mutate(model=map(.x = data,.f = ~lm(formula = value~Species,data = .))) |>
  mutate(anova=map(.x = model,.f = ~anova(.x))) |>
  mutate(HSD=map(.x = model,.f = ~HSD.test(y = .x,trt = 'Species',console = TRUE))) |>
  mutate(tidied=map(.x = model,.f = tidy))


dt|>
  pluck('anova') |> set_names(dt$name)





