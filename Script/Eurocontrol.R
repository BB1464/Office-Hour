---
  title: "TidyTemplate"
date: 2022-07-12
output: html_output
---

  # TidyTuesday

  Join the R4DS Online Learning Community in the weekly #TidyTuesday event!
Every week we post a raw dataset, a chart or article related to that dataset, and ask you to explore the data.
While the dataset will be “tamed”, it will not always be tidy! As such you might need to apply various R for Data Science techniques to wrangle the data into a true tidy format.
The goal of TidyTuesday is to apply your R skills, get feedback, explore other’s work, and connect with the greater #RStats community!
As such we encourage everyone of all skills to participate!




library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(scales)
library(ggimage)


tt_caption <- "Source: Eurocontrol  |  DataViz: Oluwafemi Oyedele (@Oluwafemoyedele)\nInspired by: https://ec.europa.eu/eurostat/web/products-eurostat-news/-/ddn-20210914-1"


# Load the weekly Data

#Dowload the weekly data and make available in the `tt` object.


tt <- tt_load("2022-07-12")


# Readme

#Take a look at the readme for the weekly data to get insight on the dataset.
#This includes a data dictionary, source, and a link to an article on the data.



# Glimpse Data

Take an initial look at the format of the data available.


tt %>%
  map(glimpse)


# Wrangle

Explore the data and process it into a nice format for plotting! Access each dataset by name by using a dollarsign after the `tt` object and then the name of the data set.


flights <- tt$flights %>%
  janitor::clean_names()



# Visualize

Using your processed dataset, create your unique visualization.

url <- "https://banner2.cleanpng.com/20180414/fcq/kisspng-airplane-icon-a5-computer-icons-flight-airplane-5ad24caf000348.7365708515237316310001.jpg"

ylim_prim <- c(70657, 668682)
ylim_sec <- c(-2.25, 0)

b <- diff(ylim_prim)/diff(ylim_sec)
a <- ylim_prim[1] - b*ylim_sec[1]

flights %>%
  filter(year > 2019) %>%
  group_by(month_mon, flt_month = floor_date(flt_date, unit = "month")) %>%
  summarize(total_departures = sum(flt_dep_1, na.rm = TRUE)) %>%
  ungroup() %>%
  inner_join(flights %>%
               filter(year == 2019) %>%
               count(month_mon, wt = flt_dep_1, name = "flt_dep_2019"),
             by = "month_mon") %>%
  mutate(pct_diff = (total_departures - flt_dep_2019)/flt_dep_2019) %>%
  ggplot(aes(x = as.Date(flt_month))) +
  geom_col(aes(y = total_departures),
           alpha = 0.75,
           fill = "royalblue") +
  geom_line(aes(y = a + pct_diff*b),
            size = 3,
            color = "white") +
  geom_emoji(aes(y = a + pct_diff*b, image = "2708"), size=.04) +
  geom_text(aes(y = a + pct_diff*b,
                label = percent(pct_diff, accuracy = 1),
                color = pct_diff > 0),
            vjust = -1,
            size = 3.5,
            fontface = "bold",
            check_overlap = TRUE,
            show.legend = FALSE) +
  scale_y_continuous("# of departures",
                     labels = comma_format(),
                     sec.axis = sec_axis(~ (. - a)/b,
                                         name = "% change from 2019",
                                         labels = percent_format())) +
  scale_x_date(date_labels = "%b-'%y", date_breaks = "3 months") +
  expand_limits(y = 750000) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "",
       title = "Commercial flights in the EU\nin 2020, 2021, and 2022",
       caption = tt_caption)



# Save Image

Save your image for sharing. Be sure to use the `#TidyTuesday` hashtag in your post on twitter!



# This will save your most recent plot
# ggsave(
#   filename = "2022_07_12_tidy_tuesday.png",
#   device = "png")

