
###########################################################################
###########################################################################
###                                                                     ###
###                          INTERRACTION PLOT                          ###
###                                                                     ###
###########################################################################
###########################################################################

library(tidyverse)

dat <- read_csv('~/../Desktop/design.csv')

dat <- dat %>%
  mutate_at(.vars = c(1:4),.funs = as.factor)

glimpse(dat)

# Recode the categorical Variable

dat <- dat %>%
  mutate(TRT1=fct_recode(TRT1,'0 L/ha Humic acid'='0',
                         '50 L/ha Humic acid'='1',
                         '100 L/ha Humic acid'='2')) %>%
  mutate(TRT2=fct_recode(TRT2,'Without CDB'='0',
                         'With CDB'='1')) %>%
  mutate(TRT3=fct_recode(TRT3,'0 t/ha'='0',
                         '20 t/ha'='1',
                         '30 t/ha'='2',
                         '600 kg/ha NPK'='3'))

# Visualization

theme_set(theme_test())

ggplot(data = dat,mapping = aes(x = TRT3,y=y14,fill=TRT1))+
  stat_summary(fun = 'mean',geom = 'col',position = 'dodge')+
  stat_summary(fun.data = mean_se,geom = 'errorbar',position = position_dodge(.9),width=.2)+
  facet_wrap(~TRT2,scales = 'free_y')+
  coord_cartesian(ylim = c(0,5),expand = c(0,0))+
  theme(axis.text.x = element_text(family = 'serif',size = 12,hjust = 1,angle = 45,face = 'bold'),strip.background  = element_rect(fill = NA),legend.position ='top',legend.title = element_blank(),axis.text.y = element_text(family = 'serif',face = 'bold',size = 12),legend.text = element_text(family = 'serif',face = 'bold',size = 12),strip.text.x = element_text(family = 'serif',face = 'bold',size = 12))+
  labs(x='Wheat straw levels',y='Total soluble solids (%)')+
  scale_fill_brewer(palette = 'Set1')

ggsave(path = )

