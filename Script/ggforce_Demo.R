library(ggforce)
library(dplyr)

dat <- dat %>% filter(Period!='P0')
dat %>% 
  ggplot(aes(x = .panel_x,y = .panel_y,group=.panel_x))+
  geom_boxplot()+labs(y='Nutrient release (%)')+
  facet_matrix(cols = vars(Incubation,Locations),rows = vars(`Nitrogen.Released.(%)`,`Phosphorus.Released.(%)`,`Potassium.Released.(%)`))+
  theme_test()






# Second Approach

dat <- dat %>% filter(Period!='P0')

dat %>% 
  ggplot(aes(x = .panel_x,y = .panel_y,group=.panel_x,))+
  geom_boxplot()+labs(y='Nutrient release (%)')+
  facet_matrix(cols = vars(Period,Incubation),rows = vars(`Nitrogen.Released.(%)`,`Phosphorus.Released.(%)`,`Potassium.Released.(%)`))+
  theme_test()


# Third Approach

dat %>% 
ggplot(aes(x = .panel_x,y = .panel_y,group=.panel_x,col=Incubation))+
  geom_point()+labs(y='Nutrient release (%)')+
  facet_matrix(vars(`Nitrogen.Released.(%)`,`Potassium.Released.(%)`,`Phosphorus.Released.(%)`))



