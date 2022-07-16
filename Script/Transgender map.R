library(tidyverse)
library(magrittr)
library(lubridate)
library(ghibli)


my_font <- "Comfortaa"
my_bkgd <- "#f5f5f2"
my_theme <- theme(text = element_text(family = my_font, color = "#22211d"),
                  rect = element_rect(fill = my_bkgd, color = NA),
                  plot.background = element_rect(fill = my_bkgd, color = NA),
                  panel.background = element_rect(fill = my_bkgd, color = NA),
                  panel.border = element_blank(),
                  legend.background = element_rect(fill = my_bkgd, color = NA),
                  legend.key = element_rect(fill = my_bkgd),
                  plot.caption = element_text(size = 6))

theme_set(theme_light() + my_theme)

thxgiv <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-11-20/thanksgiving_meals.csv")
load("~/R/data/tdor.rda")

head(thxgiv)

head(tdor)
summary(tdor)
#tdor %>% View()
tdor %<>% select(-Age_min,-Age_max)

tdor %>%
  ggplot(aes(Year)) +
  geom_histogram(binwidth=1, col=my_bkgd)


tdor %>%
  mutate(age_groups = 5 * floor(as.integer(Age)/5)) %>%
  #select(Age,age_groups)
  group_by(age_groups) %>%
  ggplot(aes(age_groups)) +
  geom_histogram(binwidth = 5, col=my_bkgd)



initials <- c("J","F","M","A","M","J","J","A","S","O","N","D")

tdor %>%
  ggplot(aes(Month)) +
  geom_bar() +
  scale_x_continuous(breaks=c(1:12), labels = initials)


tdor %>%
  count(Country,`Cause of death`, sort=T) %>%
  filter(`Cause of death` != "not reported") %>%
  group_by(Country) %>%
  mutate(perc = percent_rank(n)) %>%
  ungroup() %>%
  head(50) %>%
  ggplot(aes(Country,`Cause of death`,col=perc, size=n)) +
  geom_point() +
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5))


tdor %>%
  filter(!is.na(`Cause of death`)) %>%
  select(Country, `Cause of death`) %>%
  mutate(reported = (`Cause of death` != "not reported"),
         Country = fct_rev(fct_infreq(Country))) %>%
  ggplot(aes(fct_relevel(fct_lump(Country,10), "Other"), fill=reported)) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = ghibli_palettes$MononokeLight, name = "Reported") +
  coord_flip() +
  labs(title = "Number of Transgender Deaths - Both Reported and Unreported",
       subtitle = "By Country",
       x = "", y = "",
       caption = "tdor dataset (Transgender Day of Remembrance 2007-2018)")

ggsave("country.png")

tdor %>%
  filter(Country %in% c("Brazil","Mexico","USA")) %>%
  add_count(`Cause of death`) %>%
  mutate(`Cause of death` = fct_reorder(`Cause of death`, n)) %>%
  group_by(`Cause of death`,Country) %>%
  filter(n > 20, !is.na(`Cause of death`)) %>%
  ggplot(aes(`Cause of death`,n, fill=Country)) +
  geom_col() +
  scale_fill_manual(values = ghibli_palettes$MononokeLight) +
  scale_y_continuous(label = scales::number_format()) +
  coord_flip() +
  labs(title = "Leading Causes of Transgender Deaths",
       subtitle = "In Brazil, Mexico, and USA",
       x= "", y = "",
       caption = "tdor dataset (Transgender Day of Remembrance 2007-2018)")

ggsave("cause.png")

tdor %>%
  add_count(Country) %>%
  ggplot(aes(Longitude,Latitude,col = n)) +
  geom_point(alpha=0.4, size=0.5) +
  borders("world", colour = "#455156", size=0.25) +
  scale_color_gradient(low = "lightblue", high = "#EFF1C8", name = "Number of\nDeaths") +
  labs(x="",y="",
       title = "Number of Transgender Deaths Worldwide",
       caption = "tdor dataset (Transgender Day of Remembrance 2007-2018)") +
  theme(plot.background = element_rect(fill = "#263238"),
        panel.background = element_rect(fill = "#263238"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "#354146"),
        text = element_text(color = my_bkgd))

