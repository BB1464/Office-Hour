
#packages
library(tidyverse)
library(lubridate)
library(RColorBrewer)


#import the annual temperatures
temp_lisboa <- read_csv("https://dominicroye.github.io/files/temp_lisboa.csv")


str(temp_lisboa)


#select only the annual temperature and year column
temp_lisboa_yr <- select(temp_lisboa, YEAR, metANN)

#rename the temperature column
temp_lisboa_yr <- rename(temp_lisboa_yr, ta = metANN)

#missing values 999.9
summary(temp_lisboa_yr)


temp_lisboa_yr <- mutate(temp_lisboa_yr, ta = ifelse(ta == 999.9, NA, ta))


temp_lisboa_yr <- mutate(temp_lisboa_yr, date = str_c(YEAR, "01-01", sep = "-") %>% ymd())


# Theme

theme_strip <- theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(vjust = 3),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold")
  )


## Colour of the Strips
col_strip <- brewer.pal(11, "RdBu")

brewer.pal.info


# Plotting

ggplot(temp_lisboa_yr,
       aes(x = date, y = 1, fill = ta))+
  geom_tile()+
  scale_x_date(date_breaks = "6 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "LISBOA 1880-2018",
       caption = "Datos: GISS Surface Temperature Analysis")+
  theme_strip



## Second Step

ggplot(temp_lisboa_yr,
       aes(x = date, y = 1, fill = ta))+
  geom_col()+
  scale_x_date(date_breaks = "6 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "LISBOA 1880-2018",
       caption = "Datos: GISS Surface Temperature Analysis")+
  theme_strip


# View only the Strips

ggplot(temp_lisboa_yr,
       aes(x = date, y = 1, fill = ta))+
  geom_tile(show.legend = FALSE)+
  scale_x_date(date_breaks = "6 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  theme_void()
