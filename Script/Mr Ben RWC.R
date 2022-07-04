
############################################################################
############################################################################
###                                                                      ###
###                            EDA FOR DR BEN                            ###
###                                                                      ###
############################################################################
############################################################################



# RWC of Plant
library(tidyverse)
library(readxl)

dat <- read_excel(path = '~/../Desktop/Mr Femi.xlsx',sheet = 2)
dat <- dat |> rename(RWC=`%RWC`)
dat$RWC <- as.numeric(dat$RWC)



dat |> pivot_wider(names_from = WaterRegimes,values_from = RWC) |>
  drop_na() |>
  ggplot(aes(x = WS,y = WW),size=2)+
  geom_point()+
  labs(x='Relative Water Content under Water Stress (%)',y='Relative Water Content under Well Water (%)')+
  coord_cartesian(clip = 'off')+
    geom_smooth(se = TRUE,formula = y~x,size=1,col='red')+
  stat_poly_eq(mapping = aes(label=paste(..eq.label..,..adj.rr.label..,..p.value.label..,sep='~~~')),parse = TRUE,coef.digits = 3,p.digits = 3,small.p = TRUE)+
facet_wrap(~Location,nrow = 1,ncol = 3,scales = 'free_y')+
  theme_classic()+
  theme(panel.border  = element_rect(fill = NA,size = 1),text = element_text(family = 'serif',size = 15))


ggsave('RWC_Plant.png',path = here::here('Plot'),width = 12,height = 10,dpi = 450)


# Second Visualization for RWC for Soil

dat1 <- read_excel(path = '~/../Desktop/Mr Femi.xlsx',sheet = 3)




dat1 |>
  drop_na() |>
  ggplot(aes(x = `VWC %`,y = `RWC %`),size=2)+
  geom_point()+
  labs(x='Volumetric Water Content (%)',y='Relative Water Content  (%)')+
  coord_cartesian(clip = 'off')+
  geom_smooth(se = TRUE,formula = y~x,size=1,col='red')+
  stat_poly_eq(mapping = aes(label=paste(..eq.label..,..adj.rr.label..,..p.value.label..,sep='~~~')),parse = TRUE,coef.digits = 3,p.digits = 3,small.p = TRUE)+
  facet_wrap(~LOCATION,nrow = 1,ncol = 3,scales = 'free_y')+
  theme_classic()+
  theme(panel.border  = element_rect(fill = NA,size = 1),text = element_text(family = 'serif',size = 15))


ggsave('RWC_Soil.png',path = here::here('Plot'),width = 12,height = 10,dpi = 450)




# Correct Graph for Dr Ben
dat2 <- read_excel(path = '~/../Desktop/Mr Femi.xlsx',sheet = 1)

# Start the geoms with Rainfall


dat2 |> mutate(Month = factor(
  Month,
  levels = c(
    'Sept.2019',
    'Oct.2019',
    'Nov.2019',
    'Dec.2019',
    'Jan.2020',
    'Feb.2020',
    'Mar.2020',
    'Sept.2020',
    'Oct.2020',
    'Nov.2020',
    'Dec.2020',
    'Jan.2021',
    'Feb.2021',
    'Mar.2021'
  )
)) |>
  ggplot(aes(x = Month,y = `Rain(mm)`,fill='4'))+
  stat_summary(geom = 'col',fun = 'mean')+
  stat_summary(aes(x = Month,y = `Windspeed (km/hr)`,group=2,col='1'),geom = 'line',fun = 'mean',size=1)+
  stat_summary(aes(x = Month,y = `Temp (°c)`,group=2,col='2'),geom = 'line',fun = 'mean',size=1)+
  stat_summary(aes(x = Month,y = `Rel Hum (%)`,col='3',group=2),geom = 'line',fun = 'mean',size=1)+
  scale_color_manual(name=NULL,values = c('red','blue','green'),labels = c(
    'Wind Speed (km/hr)',
    'Temperature \u00B0C',
    'Relative Humidity (%)'),breaks = c('1','2','3'),guide=guide_legend(override.aes = list(shape=15)))+
  scale_fill_manual(name=NULL,values = c('gray50'),breaks = '4',labels='Rainfall (mm)',guide=guide_legend(direction = 'horizontal',override.aes = list(shape=15)))+scale_y_continuous(
    sec.axis = sec_axis(trans = ~ ., name = 'Temperature \u00B0C , Relative Humidity (%) and Wind Speed (km/hr)'),
    name = 'Rainfall (mm)'
  ) + scale_x_discrete(
    labels = c(
      'Sept 2019',
      'Oct 2019',
      'Nov 2019',
      'Dec 2019',
      'Jan 2020',
      'Feb 2020',
      'Mar 2020',
      'Sept 2020',
      'Oct 2020',
      'Nov 2020',
      'Dec 2020',
      'Jan 2021',
      'Feb 2021',
      'Mar 2021'
    )
  ) +coord_cartesian(expand = FALSE,ylim = c(0,250))+
  theme_classic()+
  theme(
    text = element_text(family = 'serif', size = 15),
    axis.line = element_line(size = 1),
    axis.ticks = element_line(size = 1)
  )




# Save the weather graph
ggsave('Mr_Ben_Weather.png',path = here::here('Plot'),width = 15,height = 10,dpi = 450)





