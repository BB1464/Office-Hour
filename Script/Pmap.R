library(tidyverse)

iris1 <- iris %>%
  group_by(Species) %>%
  nest() %>%
  mutate(
    gg1 = purrr::map(data, ~ ggplot(., aes(Sepal.Length, Sepal.Width)) + geom_point()),
    gg2 = purrr::map(data, ~ ggplot(., aes(Sepal.Length, Petal.Width)) + geom_point()),
    gg3 = purrr::map(data, ~ ggplot(., aes(Sepal.Length, Petal.Length)) + geom_point()),
    g = purrr::pmap(list(gg1, gg2, gg3), ~ gridExtra::grid.arrange(..1, ..2, ..3))
  )




library(gapminder)

gapminder %>%
  group_by(continent) %>%
  nest() %>%
  mutate(lm_obj = map(data, ~lm(lifeExp ~ pop + year + gdpPercap, data = .))) %>%
  mutate(lm_tidy = map(lm_obj, broom::tidy)) %>%
  ungroup() %>%
  transmute(continent, lm_tidy) %>%
  unnest(cols = c(lm_tidy))


# Import multiple excel file into R
library(tidyverse)
library(readxl)

path <- here::here('Data/Afternoon High THI.xls')

walk(
  .x = excel_sheets(path),
  .f = function(x) {
   new_nm <- tolower(x)
    assign(new_nm, read_excel(path, sheet = x), envir = .GlobalEnv)
  }
)


# purrr style lamda for reading files

walk(
  .x = excel_sheets(path),
  .f = ~ assign(tolower(.), read_excel(path, sheet = .), envir = .GlobalEnv)
)

# Map Approach to read multiple files

map(
  .x = excel_sheets(path = here::here('Data/Afternoon High THI.xls')),
  .f = ~ read_excel(
    path = here::here('Data/Afternoon High THI.xls'),
    sheet = .x
  )
)




