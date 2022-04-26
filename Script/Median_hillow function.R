library(tidyverse)

ggplot(data = iris)+
  aes(x = Sepal.Length,y = Sepal.Width)+
  geom_point()+
  facet_wrap(~Species)+geom_smooth(mapping = aes(group=Species),se = FALSE)+
  theme_classic()+
  theme(strip.text = element_blank(),
        panel.border = element_rect(colour = 'black',fill = NA,size = 1))



ggplot(data = iris)+
  aes(x = Species,y = Sepal.Length)+
  geom_jitter(width = 0.25,col='gray')+
  stat_summary(fun.data = median_hilow,col='red',size=1,fun.args = list(conf.int=0.50))+
  scale_y_continuous(limits = c(0,10),breaks = seq(0,10,2))+
  theme_classic()
