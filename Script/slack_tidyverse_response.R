# Main Question
library(tidyverse)

df<-tibble(
  red = c(1, 2,4),
  score=c(22,43,73),
  age=c(12,13,12),
  blue = c(2,4, 1),
  green=c(8,4,6),
  pink=c(5,3,4),
  id=c(1,2,3)
)



#model1<-lm(score~1+age +(1|id) + VARIABLE, data=df)





age <- c(12,13,12)
id <- c(1,2,3)
score <- c(22,43,73)
your_list <- list(red = c(1, 2,4), blue = c(2,4, 1), green=c(8,4,6), pink=c(5,3,4))

models  <- map(.x = your_list,.f =  ~ lm(score ~ 1 + age + (1|id) + .x))

tibble(models = models) %>%
  mutate(colour = names(your_list), .before = models,
         tidied_models = map(models, broom::tidy))




# Example 2

df %>%
  pivot_longer(., cols = any_of(c("red", "blue", "green", 'pink')), names_to = "color", values_to = "values") %>%
  nest(data = -color) %>%
  mutate(lm_model = map(data, ~lm(score~1+age +(1|id) + values, data=.)), tidy_lm = map(lm_model, ~broom::tidy(.)))
