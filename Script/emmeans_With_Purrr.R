###########################################################################
###########################################################################
###                                                                     ###
###                     EMMEANS WITH MULTIPLE MODEL                     ###
###                                                                     ###
###########################################################################
###########################################################################


library(tidyverse)
library(emmeans)

ds_mtcars <-
  mtcars %>%
  mutate(cyl = as.factor(cyl))

ds_nest <-
  ds_mtcars %>%
  group_by(am) %>%
  nest()

foo_model <- function(data){
  lm(hp ~ cyl, data = data)
}

ds_nest <- ds_nest %>% mutate(model = map(.x = data, .f = foo_model))

ds_temp <-
  ds_nest %>%
  mutate(
    emmeans = pmap(
      .l = list(
        object = model,
        specs = "cyl"
      ),
      .f = emmeans
    )
  )

ds_temp %>%
  mutate(emm2 = map(emmeans, data.frame)) %>%
  unnest(emm2)
