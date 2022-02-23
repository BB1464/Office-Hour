library(ggstatsplot)

names(mtcars)

ggwithinstats(data = iris,x = Species,y = Sepal.Length,type = 'parametric',pairwise.comparisons = FALSE,pairwise.display = 's',p.adjust.method = 'BH')


ggstatsplot::ggbarstats(data = iris,x = Species,y = Sepal.Length)

ggstatsplot::grouped_gghistostats(data = iris,x = Sepal.Length,grouping.var = Species)




dt <- structure(list(date = c("1/01/2000", "2/01/2000", "5/01/2000", 
                              "6/01/2000", "7/01/2000", "8/01/2000", "13/01/2000", "14/01/2000", 
                              "18/01/2000", "19/01/2000", "21/01/2000", "25/01/2000", "26/01/2000", 
                              "30/01/2000", "31/01/2000"), value = c(9L, 1L, 9L, 3L, 4L, 3L, 
                                                                     10L, 9L, 2L, 9L, 8L, 5L, 1L, 6L, 6L)), .Names = c("date", "value"
                                                                     ), row.names = c(NA, -15L), class = "data.frame")




library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)

dt2 <- dt %>%
  mutate(date = dmy(date)) %>%
  mutate(cumsum = cumsum(value)) %>% # Stop Here for my weather data
  complete(date = full_seq(date, period = 1), fill = list(value = 0)) %>%
  mutate(cum_rolling10 = rollapplyr(value, width = 0, FUN = sum, partial = TRUE)) %>%
  drop_na(cumsum)
dt2
