library(gapminder)

gapminder %>%
  group_by(continent) %>%
  nest() %>%
  mutate(lm_obj = map(data, ~lm(lifeExp ~ pop + year + gdpPercap, data = .))) %>%
  mutate(lm_tidy = map(lm_obj, broom::tidy)) %>%
  ungroup() %>%
  transmute(continent, lm_tidy) %>%
  unnest(cols = c(lm_tidy))
