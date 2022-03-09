
###########################################################################
###########################################################################
###                                                                     ###
###                     IMPORT THE REQUIRED LIBRARY                     ###
###                                                                     ###
###########################################################################
###########################################################################

library(tidyverse)
library(readxl)
library(agricolae)


dat <- read_excel('Data/NaCl experiment data.xlsx')

dat <- dat %>%
  mutate_at(.vars = c(1:3),.funs = as.factor)

dat <- dat %>%
  mutate_if(is.character,as.numeric)

dat <- dat %>%
  mutate_if(is.character,as.numeric)


glimpse(dat)



###########################################################################
###########################################################################
###                                                                     ###
###                      TWO WAY INTERRACTION PLOT                      ###
###                                                                     ###
###########################################################################
###########################################################################


# Order the factors of NACL
dat$NaCl <- factor(dat$NaCl,levels = c('0 mM','50 mM','160 mM'))



# Salt Tolerance

ggplot(data = dat,aes(x = Cultivar,y=ST, fill = NaCl))+
  stat_summary(geom = 'col',position = 'dodge',fun = 'mean')+
  stat_summary(geom = 'errorbar',fun.data = 'mean_se',position = position_dodge(.9),width=.7,colour='black')+
  scale_fill_brewer(palette = 'Set1')+
  theme_classic()+
  coord_cartesian(expand = c(0,0))+
  ylab('Salt Tolerance')+
  theme(text = element_text(family = 'serif',face = 'bold',size = 12))


# Save the Output
 ggsave('ST.png',width = 9,height = 7,dpi = 300)




 # Salt Tolerance Index

 ggplot(data = dat,aes(x = Cultivar,y=STI, fill = NaCl))+
   stat_summary(geom = 'col',position = 'dodge',fun = 'mean')+
   stat_summary(geom = 'errorbar',fun.data = 'mean_se',position = position_dodge(.9),width=.7,colour='black')+
   scale_fill_brewer(palette = 'Set1')+
   theme_classic()+
   coord_cartesian(expand = c(0,0))+
   ylab('Salt Tolerance Index')+
   theme(text = element_text(family = 'serif',face = 'bold',size = 12))



 # Save the Output
 ggsave('STI.png',width = 9,height = 7,dpi = 300)



 # Stress Weighted Performance

 ggplot(data = dat,aes(x = Cultivar,y=SWP, fill = NaCl))+
   stat_summary(geom = 'col',position = 'dodge',fun = 'mean')+
   stat_summary(geom = 'errorbar',fun.data = 'mean_se',position = position_dodge(.9),width=.7,colour='black')+
   scale_fill_brewer(palette = 'Set1')+
   theme_classic()+
   coord_cartesian(expand = c(0,0))+
   ylab('Stress Weighted Performance')+
   theme(text = element_text(family = 'serif',face = 'bold',size = 12))



 # Save the Output
 ggsave('SWP.png',width = 9,height = 7,dpi = 300)
