
############################################################################
############################################################################
###                                                                      ###
###                            PIE CHART IN R                            ###
###                                                                      ###
############################################################################
############################################################################

library(tidyverse)

df <- data.frame(Group=c('Tall','Short','Average'),Height=c(14,30,45))


df %>%
  mutate(Percentage=round(prop.table(Height)*100,1)) %>%
  ggplot(aes(x = '',y = Percentage,fill=Group))+
  geom_bar(stat = 'identity',width = 1)+
  coord_polar(theta = 'y',direction = 1)+
  geom_text(aes(label=paste0(Percentage,'%')),position = position_stack(vjust = 0.5))+
  theme_void()+
  scale_fill_brewer(palette = 'Set1')


