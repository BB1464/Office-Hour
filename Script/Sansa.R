
# Import the Required Packages --------------------------------------------


library(tidyverse)
library(readxl)
library(lubridate)

Temp <- read_excel(path = here::here('Data/Moisture and Temperature Data YR2.xlsx'),sheet = 2)


# Example 1 -------------------------------------------------



Temp |>
  mutate(Month=month(Time,label = TRUE,abbr = TRUE)) |>
  mutate(Month=factor(Month)) |>
  mutate(Year=year(Time)) |>
  unite(col = Month,c('Month','Year'),sep = '-') |>
  mutate(Month=factor(Month,levels = c('Oct-2020','Nov-2020','Dec-2020','Jan-2021','Feb-2021','Mar-2021'))) |>
  drop_na() |>
ggplot(aes(x = Month,y = `Celsius(°C)`,group=2))+
  stat_summary(geom = 'point',fun = mean,size=2,col='green',shape=19)+
  stat_summary(mapping = aes(x = Month,y = `Humidity(%rh)`,group=2),geom = 'point',fun = mean,size=2,col='red',shape=19)+
  stat_summary(mapping = aes(x = Month,y = `Humidity(%rh)`,group=2),geom = 'line',fun = mean,size=0.8,col='red')+
stat_summary(geom = 'line',fun = mean,size=0.8,col='green')+
facet_wrap(~Location,nrow = 1)+
scale_y_continuous(sec.axis = sec_axis(trans = ~.,breaks = seq(0,120,5),labels = seq(0,120,5),name = 'Relative Humidity (%)'))+
theme_bw()+
theme(strip.background = element_rect(fill = 'white',colour = NULL))+
  labs(y='Temperature (\u00B0 C)')+
  theme(axis.title.y.left = element_text(family = 'serif',face = 'bold',colour = 'green',size = 14,margin = margin(t = 0,r = 10,b = 0,l = 0)),axis.title.y.right = element_text(family = 'serif',face = 'bold',colour = 'red',size = 14,margin = margin(t = 0,r = 0,b = 0,l = 10)),axis.line.y.left = element_line(colour = 'green',size=0.8),axis.line.y.right = element_line(colour = 'red',size=0.8),axis.ticks.y.left = element_line(colour = 'green',size = 0.8,linetype = 'solid'),axis.ticks.y.right = element_line(colour = 'red',size = 0.8,linetype = 'solid'),axis.text.y.left = element_text(family = 'serif',face = 'bold',colour = 'green',size = 14),axis.text.y.right = element_text(family = 'serif',face = 'bold',colour = 'red',size = 14),axis.title.x = element_text(family = 'serif',size = 14,face = 'bold',colour = 'black',),axis.text.x = element_text(family = 'serif',size = 14,face = 'bold',colour = 'black',angle = 45,hjust = 1),strip.text.x = element_text(family = 'serif',face = 'bold',colour = 'black',size = 14))



# Save the Plot ----------------------------------------------


ggsave('clim1.png',width = 9,height = 9,dpi = 270,bg = 'white',path = here::here('Plot'))



# Example 2 ---------------------------------------------------------------

Temp |>
  mutate(Month=month(Time,label = TRUE,abbr = TRUE)) |>
  mutate(Year=year(Time)) |>
  unite(col = Month,c('Month','Year'),sep = '-') |>
  mutate(Month=factor(Month,levels = c('Oct-2020','Nov-2020','Dec-2020','Jan-2021','Feb-2021','Mar-2021'))) |>
  rename('Temperature (\u00B0C)'=`Celsius(°C)`) |>
  rename(`Relative Humidity (%)`=`Humidity(%rh)`) |>
  rename('Dew Point (\u00B0C)'=`dew point(°C)`) |>
  pivot_longer(cols = c('Temperature (\u00B0C)',`Relative Humidity (%)`,'Dew Point (\u00B0C)')) |>
  drop_na() |>
  ggplot(aes(x = Month,y = value,group=2,col=name))+
  stat_summary(geom = 'point',fun = mean,size=2.5,shape=19)+
  stat_summary(geom = 'line',fun = mean,size=1)+
  facet_grid(name~Location,scales = 'free_y',switch = 'y',space = 'fixed')+
  theme_bw()+
  theme(legend.title = element_blank(),legend.position = c(0.1,0.8),legend.background = element_blank(),legend.key = element_blank())+
  labs(y='')+
  theme(strip.background = element_rect(fill = 'gray95',colour = NULL),text=element_text(family = 'serif',face = 'bold',colour = 'black',size = 14),axis.text.x = element_text(family = 'serif',face = 'bold',colour = 'black',size = 14,angle = 45,hjust = 1),strip.text.x = element_text(family = 'serif',face = 'bold',colour = 'black',size = 15))+
  scale_color_brewer(palette = 'Dark2')


# Save the Plot -----------------------------------------------


ggsave('clim2.png',width = 11,height = 9,dpi = 270,bg = 'white',path = here::here('Plot'))



# Import the Second Sheet -------------------------------------------------


Soil <- read_excel(path = here::here('Data/Moisture and Temperature Data YR2.xlsx'),sheet = 'Soil moisture Data')


# Data Wrangling ----------------------------------------------------------


Soil |>
  pivot_longer(cols = c(Initial,`2wks after`,`4wks after`),values_to = 'Soil Moisture Content (%)') |>
  mutate(name=factor(name,levels = c('Initial','2wks after','4wks after'))) |>
  ggplot(aes(x = name,y = `Soil Moisture Content (%)`,fill=name))+
  stat_boxplot(geom = 'errorbar')+
geom_boxplot(outlier.shape = NA)+
  scale_fill_brewer(palette = 'Dark2',name='')+
  theme_bw()+guides(fill='none')+
  facet_grid(~Location)+
  theme(text=element_text(family = 'serif',face = 'bold',colour = 'black',size = 14),axis.text.x = element_text(family = 'serif',face = 'bold',colour = 'black',size = 14),axis.text.y = element_text(family = 'serif',face = 'bold',colour = 'black',size = 14),axis.ticks = element_line(size = 0.7,linetype = 'solid'))+
  labs(x='')


# Save the Plot --------------------------------------------------------

ggsave('clim3.png',width = 10,height = 10,dpi = 270,bg = 'white',path = here::here('Plot'))
