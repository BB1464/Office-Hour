


library(tidyverse)
library(ggpmisc)
library(ggtext)


ggplot(data = iris,mapping = aes(x = Sepal.Length,y = Sepal.Width))+
  geom_smooth(se = FALSE,formula = y~x)+
  stat_poly_eq(mapping = aes(label=paste(..eq.label..,..adj.rr.label..,..p.value.label..,sep='~~~')),parse = TRUE,coef.digits = 3,p.digits = 3,small.p = TRUE)+theme_light()



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

dat <- dat |> rename(RWC=`%RWC`)

dat$RWC <- as.numeric(dat$RWC)



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
