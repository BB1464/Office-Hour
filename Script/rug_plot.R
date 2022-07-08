
library(tidyverse)

url <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/ZonAnn.Ts+dSST.csv"

dat <- read_csv(url)

zonal <- rev(c("64N-90N", "44N-64N", "24N-44N", "EQU-24N", "24S-EQU", "44S-24S", "64S-44S", "90S-64S"))

temp <- dat |>
  select(year=Year,all_of(zonal)) |>
  pivot_longer(-year,names_to = 'zones',values_to = 't_diff') |>
  mutate(zones=factor(zones,levels = zonal)) |>
  mutate(zon=as.numeric(zones))

temp_y <- temp |>
  filter(year=='2021')

ggplot(data = temp,aes(x = t_diff,xend=t_diff,y=zon-0.25,yend=zon+0.25))+
  geom_segment(col='black',alpha=0.25,size=2)+
  geom_segment(data = temp_y,aes(col=t_diff),size=3,lineend = 'round')+
  scale_color_gradient2(low = 'darkblue',mid = 'white',high = 'darkred',midpoint = 0,guide = 'none')+
  scale_y_continuous(breaks =1:8,labels = zonal)+
  scale_x_continuous(breaks = seq(-3,4,1),labels = seq(-3,4,1),limits = c(-3,4))+
  labs(x='Temperature anomally (\u00B0 C)',y=NULL,title = 'Variation in Annaual Temperature Annomally (1880-2021)',subtitle = 'Bars for 2021 are Colored by the Size of Annomally')+coord_cartesian(expand = FALSE,clip = 'off')+
  theme(plot.background = element_rect(colour = 'black'),panel.background = element_rect(fill='white'),axis.line = element_line(size=1),axis.ticks = element_blank(),plot.title.position  = 'plot')





