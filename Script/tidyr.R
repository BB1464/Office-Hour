
library(tidyverse)


country <- c("afg", "alb", "aus")
gdp_2018 <- c(123, 532, 555)
gdp_2019 <- c(233, 531, 621)
gdp_2020 <- c(112, 231, 323)
inf_2018 <- c(0.1, 0.2, 0.3)
inf_2019 <- c(0.01, 0.01, 0.2)
inf_2020 <- c(0.5, 0.4, 0.4)

df <- cbind.data.frame(country, gdp_2018, gdp_2019, gdp_2020,
                       inf_2018, inf_2019, inf_2020)



country <- c("afg","afg","afg", "alb","alb","alb", "aus", "aus", "aus")
year <- c(2018, 2019, 2020, 2018, 2019, 2020, 2018, 2019, 2020)
gdp <- c(123, 532, 555,
         233, 531, 621,112, 231, 323)
inf <- c(0.1, 0.2, 0.3, 0.01, 0.01, 0.2, 0.5, 0.4, 0.4)

long_df <- cbind.data.frame(country, year, gdp, inf)



library(tidyr)
df %>%
  pivot_longer(-country, names_to = c(".value", "year"), names_sep = "_")



myvar = tibble(var_1 = c("a", "b", "c"),
               var_2 = c("d", "e", "f"),
               var_3 = c("g", "h", "i"),
               something_var_1 = 1:3,
               something_var_2 = 4:6,
               something_var_3 = 7:9)



myvar %>%
  pivot_longer(everything(),
               names_to = c(".value", "set"),
               names_pattern = "(.+)_(.)")



myvar %>%
  pivot_longer(everything(),
               names_to = c(".value", "set"),
               names_sep = "_(?=\\d)")



