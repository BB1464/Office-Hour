library(tidyverse)
library(broom)

mtcars %>%
  nest(-am) %>%
  mutate(am = factor(am, levels = c(0, 1), labels = c("automatic", "manual")),
         fit = map(data, ~ lm(mpg ~ hp + wt + disp, data = .)),
         results = map(fit, augment)) %>%
  unnest(results) %>%
  ggplot(aes(x = mpg, y = .fitted)) +
  geom_abline(intercept = 0, slope = 1, alpha = .2) +  # Line of perfect fit
  geom_point() +
  facet_grid(am ~ .) +
  labs(x = "Miles Per Gallon", y = "Predicted Value") +
  theme_bw()


mtcars %>% nest(-cyl)


d <- mtcars %>% nest(-cyl)
d$data[d$cyl == 4]


mtcars %>% nest(-cyl, -am)


mtcars %>%
  nest(-cyl) %>%
  mutate(fit = map(data, ~ t.test(.$mpg)))


d <- mtcars %>%
  nest(-cyl) %>%
  mutate(fit = map(data, ~ t.test(.$mpg)))

d$fit[d$cyl == 4]


mtcars %>%
  nest(-cyl) %>%
  mutate(fit = map(data, ~ t.test(.$mpg)),
         p   = map_dbl(fit, "p.value"))


mtcars %>%
  nest(-cyl) %>%
  mutate(fit = map(data, ~ t.test(.$mpg)),
         results = map(fit, glance))


mtcars %>%
  nest(-cyl) %>%
  mutate(fit = map(data, ~ t.test(.$mpg)),
         results = map(fit, glance)) %>%
  unnest(results)


mtcars %>%
  nest(-cyl) %>%
  mutate(fit = map(data, ~ t.test(.$mpg)),
         results = map(fit, glance)) %>%
  unnest(results) %>%
  ggplot(aes(x = factor(cyl), y = estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .2) +
  labs(x = "Cylinders (cyl)", y = "Miles Per Gallon (mpg)")



mtcars %>%
  nest(-cyl) %>%
  mutate(fit = map(data, ~ lm(mpg ~ hp + wt + disp, data = .)))



mtcars %>%
  nest(-cyl) %>%
  mutate(fit = map(data, ~ lm(mpg ~ hp + wt + disp, data = .)),
         results = map(fit, glance)) %>%
  unnest(results)



mtcars %>%
  nest(-cyl) %>%
  mutate(fit = map(data, ~ lm(mpg ~ hp + wt + disp, data = .)),
         results = map(fit, glance)) %>%
  unnest(results) %>%
  ggplot(aes(x = factor(cyl), y = r.squared)) +
  geom_bar(stat = "identity") +
  labs(x = "Cylinders", y = expression(R^{2}))




mtcars %>%
  nest(-cyl) %>%
  mutate(fit = map(data, ~ lm(mpg ~ hp + wt + disp, data = .)),
         results = map(fit, augment))



mtcars %>%
  nest(-cyl) %>%
  mutate(fit = map(data, ~ lm(mpg ~ hp + wt + disp, data = .)),
         results = map(fit, augment)) %>%
  unnest(results)



mtcars %>%
  nest(-cyl) %>%
  mutate(fit = map(data, ~ lm(mpg ~ hp + wt + disp, data = .)),
         results = map(fit, augment)) %>%
  unnest(results) %>%
  ggplot(aes(x = mpg, y = .fitted)) +
  geom_abline(intercept = 0, slope = 1, alpha = .2) +  # Line of perfect fit
  geom_point() +
  facet_grid(cyl ~ .) +
  theme_bw()


