
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

# I modified this script
ggplot(data = dat,aes(x = Cultivar,y=ST, fill = NaCl))+
  stat_summary(geom = 'col',position = 'dodge',fun = 'mean')+
 stat_summary(geom = 'errorbar',fun.data = 'mean_se',position = position_dodge(width = 0.9,preserve = NULL),width=.6,colour='black',size=0.7)+
  scale_fill_manual(name='NaCl',values = c("#dfc27d", "#D4D4D4", "#737373"))+
  theme_classic()+
  coord_cartesian(expand = c(0,0))+
  ylab('Salt Tolerance')+
  theme(text = element_text(family = 'serif',face = 'bold',size = 12),legend.position = c(0.1,0.79),legend.title = element_text(family = 'serif',face = 'bold',size = 12),legend.text = element_text(family = 'serif',face = 'bold',size = 14),legend.background = element_blank(),axis.line = element_line(colour = 'black',size = 1),axis.ticks = element_line(colour = 'black',size = 1))


# Save the Output
 ggsave('ST.png',width = 9,height = 7,dpi = 300)



 # Salt Tolerance Index

 ggplot(data = dat,aes(x = Cultivar,y=STI, fill = NaCl))+
   stat_summary(geom = 'col',position = 'dodge',fun = 'mean')+
   stat_summary(geom = 'errorbar',fun.data = 'mean_se',position = position_dodge(.9),width=.7,colour='black')+
  scale_fill_manual(name='NaCl',values = c("#dfc27d", "#D4D4D4", "#737373"))+
   theme_classic()+
   coord_cartesian(expand = c(0,0))+
   ylab('Salt Tolerance Index')+
   theme(text = element_text(family = 'serif',face = 'bold',size = 12),legend.position = c(0.1,0.79),legend.title = element_text(family = 'serif',face = 'bold',size = 12),legend.text = element_text(family = 'serif',face = 'bold',size = 14),legend.background = element_blank(),axis.line = element_line(colour = 'black',size = 1),axis.ticks = element_line(colour = 'black',size = 1))



 # Save the Output
 ggsave('STI.png',width = 9,height = 7,dpi = 300)



 # Stress Weighted Performance

 ggplot(data = dat,aes(x = Cultivar,y=SWP, fill = NaCl))+
   stat_summary(geom = 'col',position = 'dodge',fun = 'mean')+
   stat_summary(geom = 'errorbar',fun.data = 'mean_se',position = position_dodge(.9),width=.7,colour='black')+
scale_fill_manual(name='NaCl',values = c("#dfc27d", "#D4D4D4", "#737373"))+
   theme_classic()+
   coord_cartesian(expand = c(0,0))+
   ylab('Stress Weighted Performance')+
   theme(text = element_text(family = 'serif',face = 'bold',size = 12),legend.position = 'top',legend.title = element_text(family = 'serif',face = 'bold',size = 12),legend.text = element_text(family = 'serif',face = 'bold',size = 14),legend.background = element_blank(),axis.line = element_line(colour = 'black',size = 1),axis.ticks = element_line(colour = 'black',size = 1))



 # Save the Output
 ggsave('SWP.png',width = 9,height = 7,dpi = 300)


 # Chl a

 dat$NaCl <- dat$NaCl <- factor(dat$NaCl,levels = c('0 mM','50 mM','160 mM'))

 ggplot(data = dat,aes(x = Cultivar,y=`Chl a`, fill = NaCl))+
   stat_summary(geom = 'col',position = 'dodge',fun = 'mean')+
   stat_summary(geom = 'errorbar',fun.data = 'mean_se',position = position_dodge(.9),width=.7,colour='black')+
  scale_fill_manual(name='NaCl',values = c("#dfc27d", "#D4D4D4", "#737373"))+
   theme_classic()+
   coord_cartesian(expand = c(0,0),ylim = c(0,2.5))+
   ylab('Chl a')+
   theme(text = element_text(family = 'serif',face = 'bold',size = 12),legend.position = 'top',legend.title = element_text(family = 'serif',face = 'bold',size = 12),legend.text = element_text(family = 'serif',face = 'bold',size = 14),legend.background = element_blank(),axis.line = element_line(colour = 'black',size = 1),axis.ticks = element_line(colour = 'black',size = 1))



 # Save the Output
 ggsave('Chl a.png',width = 9,height = 7,dpi = 300)



 # Chl b

 ggplot(data = dat,aes(x = Cultivar,y=`Chl b`, fill = NaCl))+
   stat_summary(geom = 'col',position = 'dodge',fun = 'mean')+
   stat_summary(geom = 'errorbar',fun.data = 'mean_se',position = position_dodge(.9),width=.7,colour='black')+
scale_fill_manual(name='NaCl',values = c("#dfc27d", "#D4D4D4", "#737373"))+
   theme_classic()+
   coord_cartesian(expand = c(0,0),ylim = c(0,2.5))+
   ylab('Chl b')+
  theme(text = element_text(family = 'serif',face = 'bold',size = 12),legend.position = 'top',legend.title = element_text(family = 'serif',face = 'bold',size = 12),legend.text = element_text(family = 'serif',face = 'bold',size = 14),legend.background = element_blank(),axis.line = element_line(colour = 'black',size = 1),axis.ticks = element_line(colour = 'black',size = 1))



 # Save the Output
 ggsave('Chl b.png',width = 9,height = 7,dpi = 300)




 # Chl a/b

 ggplot(data = dat,aes(x = Cultivar,y=`ChI a/b`, fill = NaCl))+
   stat_summary(geom = 'col',position = 'dodge',fun = 'mean')+
   stat_summary(geom = 'errorbar',fun.data = 'mean_se',position = position_dodge(.9),width=.7,colour='black')+
scale_fill_manual(name='NaCl',values = c("#dfc27d", "#D4D4D4", "#737373"))+
   theme_classic()+
   coord_cartesian(expand = c(0,0))+
   ylab('Chl a/b')+
 theme(text = element_text(family = 'serif',face = 'bold',size = 12),legend.position = 'top',legend.title = element_text(family = 'serif',face = 'bold',size = 12),legend.text = element_text(family = 'serif',face = 'bold',size = 14),legend.background = element_blank(),axis.line = element_line(colour = 'black',size = 1),axis.ticks = element_line(colour = 'black',size = 1))



 # Save the Output
 ggsave('Chl a_b.png',width = 9,height = 7,dpi = 300)




 # Carotenoid

 ggplot(data = dat,aes(x = Cultivar,y=Carotenoid, fill = NaCl))+
   stat_summary(geom = 'col',position = 'dodge',fun = 'mean')+
   stat_summary(geom = 'errorbar',fun.data = 'mean_se',position = position_dodge(.9),width=.5,colour='black')+
scale_fill_manual(name='NaCl',values = c("#dfc27d", "#D4D4D4", "#737373"))+
   theme_classic()+
   coord_cartesian(expand = c(0,0))+
   ylab('Carotenoid')+
  theme(text = element_text(family = 'serif',face = 'bold',size = 12),legend.position = 'top',legend.title = element_text(family = 'serif',face = 'bold',size = 12),legend.text = element_text(family = 'serif',face = 'bold',size = 14),legend.background = element_blank(),axis.line = element_line(colour = 'black',size = 1),axis.ticks = element_line(colour = 'black',size = 1))



 # Save the Output
 ggsave('Carotenoid.png',width = 9,height = 7,dpi = 300)
