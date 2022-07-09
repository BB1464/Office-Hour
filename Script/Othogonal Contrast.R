
# Othogonal Contrast

library(tidyverse)
library(readxl)
library(emmeans)


mtcars$cyl <-as.factor(mtcars$cyl)

fit <- lm(mpg ~ wt + cyl, mtcars)

fit

emm_fit <- emmeans(fit, specs = "cyl")
pairs(x = emm_fit)

#contrast(object = emm_fit,method = 'pairwise')
