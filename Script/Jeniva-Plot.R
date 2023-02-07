
# Import Required Packages ------------------------------------------------

library(tidyverse)
library(readxl)



# Import the dataset ------------------------------------------------------

dat <- read_excel(path = here::here('Data/CONVERTED TABLES.xlsx'),sheet = 1)


dat <- dat |>
  mutate(Incubation=factor(Incubation,levels = c('7 days','14 days','21 days','28 days','35 days'))) |>
  mutate(Treatment=factor(Treatment,levels = c('200g soil + 0ml POME','200g soil + 50ml POME','200g soil + 70ml POME','200g soil + 90ml POME')))


pal <- RColorBrewer::brewer.pal(n = 4,name = 'RdBu')

# pH

ggplot(data = dat,aes(x = Incubation,y = pH,fill=Treatment))+
  stat_summary(geom = 'errorbar',position = position_dodge(.9),fun.data = mean_se)+
  stat_summary(geom = 'col',position = position_dodge(.9),fun = mean)+
  scale_fill_manual(values = pal)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0),add = c(0,0.1)))+
  theme(text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12))


## Save the Plot

ggsave('pH.png',path = here::here('Plot/Jeniva'),width = 8,height = 8,dpi = 320)

## EC

ggplot(data = dat,aes(x = Incubation,y = EC,fill=Treatment))+
  stat_summary(geom = 'errorbar',position = position_dodge(.9),fun.data = mean_se)+
  stat_summary(geom = 'col',position = position_dodge(.9),fun = mean)+
  scale_fill_manual(values = pal)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0.1),add = c(0,0.9)))+
  theme(text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12))+
  labs(y='Electrical Conductivity')

## Save the Plot

ggsave('EC.png',path = here::here('Plot/Jeniva'),width = 8,height = 8,dpi = 320)


## TN

ggplot(data = dat,aes(x = Incubation,y = TN,fill=Treatment))+
  stat_summary(geom = 'errorbar',position = position_dodge(.9),fun.data = mean_se)+
  stat_summary(geom = 'col',position = position_dodge(.9),fun = mean)+
  scale_fill_manual(values = pal)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0),add = c(0,0.1)))+
  theme(text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12))+
  labs(y='Total Nitrogen')

## Save the Plot

ggsave('TN.png',path = here::here('Plot/Jeniva'),width = 8,height = 8,dpi = 320)


## OC

ggplot(data = dat,aes(x = Incubation,y = OC,fill=Treatment))+
  stat_summary(geom = 'errorbar',position = position_dodge(.9),fun.data = mean_se)+
  stat_summary(geom = 'col',position = position_dodge(.9),fun = mean)+
  scale_fill_manual(values = pal)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0.1),add = c(0,0.1)))+
  theme(text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12))+
  labs(y='Organic Carbon')

## Save the Plot

ggsave('OC.png',path = here::here('Plot/Jeniva'),width = 8,height = 8,dpi = 320)


## OM

ggplot(data = dat,aes(x = Incubation,y = OM,fill=Treatment))+
  stat_summary(geom = 'errorbar',position = position_dodge(.9),fun.data = mean_se)+
  stat_summary(geom = 'col',position = position_dodge(.9),fun = mean)+
  scale_fill_manual(values = pal)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0.1),add = c(0,0.1)))+
  theme(text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12))+
  labs(y='Organic matter')


## Save the Plot

ggsave('OM.png',path = here::here('Plot/Jeniva'),width = 8,height = 8,dpi = 320)



## Available Phosphorus

ggplot(data = dat,aes(x = Incubation,y = `Av. P`,fill=Treatment))+
  stat_summary(geom = 'errorbar',position = position_dodge(.9),fun.data = mean_se)+
  stat_summary(geom = 'col',position = position_dodge(.9),fun = mean)+
  scale_fill_manual(values = pal)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0.2),add = c(0,0.1)))+
  theme(text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12))+
  labs(y='Available Phosphorus')

## Save the Plot

ggsave('Avail.P.png',path = here::here('Plot/Jeniva'),width = 8,height = 8,dpi = 320)


## Calcium

ggplot(data = dat,aes(x = Incubation,y = Ca,fill=Treatment))+
  stat_summary(geom = 'errorbar',position = position_dodge(.9),fun.data = mean_se)+
  stat_summary(geom = 'col',position = position_dodge(.9),fun = mean)+
  scale_fill_manual(values = pal)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0.2),add = c(0,0.1)))+
  theme(text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12))+
  labs(y='Calcium')

## Save the Plot

ggsave('Ca.png',path = here::here('Plot/Jeniva'),width = 8,height = 8,dpi = 320)


## Magnessium

ggplot(data = dat,aes(x = Incubation,y = Mg,fill=Treatment))+
  stat_summary(geom = 'errorbar',position = position_dodge(.9),fun.data = mean_se)+
  stat_summary(geom = 'col',position = position_dodge(.9),fun = mean)+scale_fill_manual(values = pal)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0.2),add = c(0,0.1)))+theme(text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12))+
  labs(y='Magnessium')

## Save the Plot

ggsave('Mg.png',path = here::here('Plot/Jeniva'),width = 8,height = 8,dpi = 320)


## Potassium

ggplot(data = dat,aes(x = Incubation,y = K,fill=Treatment))+
  stat_summary(geom = 'errorbar',position = position_dodge(.9),fun.data = mean_se)+
  stat_summary(geom = 'col',position = position_dodge(.9),fun = mean)+scale_fill_manual(values = pal)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0.2),add = c(0,0.1)))+theme(text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12))+
  labs(y='Potassium')

## Save the Plot

ggsave('Potassium.png',path = here::here('Plot/Jeniva'),width = 8,height = 8,dpi = 320)


## Sodium

ggplot(data = dat,aes(x = Incubation,y = Na,fill=Treatment))+
  stat_summary(geom = 'errorbar',position = position_dodge(.9),fun.data = mean_se)+
  stat_summary(geom = 'col',position = position_dodge(.9),fun = mean)+
  scale_fill_manual(values = pal)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0.2),add = c(0,0.1)))+
  theme(text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12))+
  labs(y='Sodium')

## Save the Plot

ggsave('Na.png',path = here::here('Plot/Jeniva'),width = 8,height = 8,dpi = 320)


## Hydrogen
ggplot(data = dat,aes(x = Incubation,y = H,fill=Treatment))+
  stat_summary(geom = 'errorbar',position = position_dodge(.9),fun.data = mean_se)+
  stat_summary(geom = 'col',position = position_dodge(.9),fun = mean)+
  scale_fill_manual(values = pal)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0.2),add = c(0,0.1)))+
  theme(text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12))

## Save the Plot

ggsave('H.png',path = here::here('Plot/Jeniva'),width = 8,height = 8,dpi = 320)


## Alluminium

ggplot(data = dat,aes(x = Incubation,y = `Al3+`,fill=Treatment))+
  stat_summary(geom = 'errorbar',position = position_dodge(.9),fun.data = mean_se)+
  stat_summary(geom = 'col',position = position_dodge(.9),fun = mean)+
  scale_fill_manual(values = pal)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0.2),add = c(0,0.1)))+
  theme(text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12))


## Save the Plot

ggsave('Al.png',path = here::here('Plot/Jeniva'),width = 8,height = 8,dpi = 320)


## ECEC

ggplot(data = dat,aes(x = Incubation,y = ECEC,fill=Treatment))+
  stat_summary(geom = 'errorbar',position = position_dodge(.9),fun.data = mean_se)+
  stat_summary(geom = 'col',position = position_dodge(.9),fun = mean)+
  scale_fill_manual(values = pal)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0.2),add = c(0,0.1)))+
  theme(text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12))

## Save the Plot

ggsave('ECEC.png',path = here::here('Plot/Jeniva'),width = 8,height = 8,dpi = 320)



# Import the dataset ------------------------------------------------------

dat1 <- read_excel(path = here::here('Data/CONVERTED TABLES.xlsx'),sheet = 2)


## Plot

ggplot(data = dat1,aes(x = Incubation,y = `Bacteria (CFU/g)`,fill=Treatment))+stat_summary(geom = 'errorbar',position = position_dodge(.9),fun.data = mean_se)+
  stat_summary(geom = 'col',position = position_dodge(.9),fun = mean)+scale_fill_manual(values = pal)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0.2),add = c(0,0.1)))+theme(text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12))

## Save the Plot

ggsave('bacteria.png',path = here::here('Plot/Jeniva'),width = 8,height = 8,dpi = 320)


ggplot(data = dat1,aes(x = Incubation,y = `Fungi (CFU/g)`,fill=Treatment))+stat_summary(geom = 'errorbar',position = position_dodge(.9),fun.data = mean_se)+
  stat_summary(geom = 'col',position = position_dodge(.9),fun = mean)+scale_fill_manual(values = pal)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0.2),add = c(0,0.1)))+theme(text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12))

## Save the Plot

ggsave('fungi.png',path = here::here('Plot/Jeniva'),width = 8,height = 8,dpi = 320)

ggplot(data = dat1,aes(x = Incubation,y = `NB (CFU/g)`,fill=Treatment))+stat_summary(geom = 'errorbar',position = position_dodge(.9),fun.data = mean_se)+
  stat_summary(geom = 'col',position = position_dodge(.9),fun = mean)+scale_fill_manual(values = pal)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0.2),add = c(0,0.1)))+theme(text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12))

## Save the Plot

ggsave('nb.png',path = here::here('Plot/Jeniva'),width = 8,height = 8,dpi = 320)


ggplot(data = dat1,aes(x = Incubation,y = `PSB (CFU/g)`,fill=Treatment))+stat_summary(geom = 'errorbar',position = position_dodge(.9),fun.data = mean_se)+
  stat_summary(geom = 'col',position = position_dodge(.9),fun = mean)+scale_fill_manual(values = pal)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0.2),add = c(0,0.1)))+theme(text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12))

## Save the Plot

ggsave('psb.png',path = here::here('Plot/Jeniva'),width = 8,height = 8,dpi = 320)

ggplot(data = dat1,aes(x = Incubation,y = `LB (CFU/g)`,fill=Treatment))+stat_summary(geom = 'errorbar',position = position_dodge(.9),fun.data = mean_se)+
  stat_summary(geom = 'col',position = position_dodge(.9),fun = mean)+scale_fill_manual(values = pal)+
  theme_bw()+
  scale_y_continuous(expand = expansion(mult = c(0,0.2),add = c(0,0.1)))+theme(text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12))

## Save the Plot

ggsave('lb.png',path = here::here('Plot/Jeniva'),width = 8,height = 8,dpi = 320)




