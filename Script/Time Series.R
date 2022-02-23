
# Time Series Plot

quickleaf <- read.csv('https://github.com/BB1464/Time-Series-plot-with-SED/raw/master/2021-08-06%20leaf%20biomass_copy%20paste.csv')




#Example of graph with error band and SED at the top!!!
library(ggplot2)
library(readxl)

# Quick graph to choose between quicklmer() and quicklmer2(
#quickleaf=read.csv('2021-08-06 leaf biomass_copy paste.csv')

quickleaf=quickleaf %>% mutate_if(is.character,as.factor)

ggplot(quickleaf,aes(reorder(Month,Order),Avg,group=Treatment))+  #2 methods compared
  geom_ribbon(data=quickleaf,aes(ymin=Avg-SE,ymax=Avg+SE,fill=Treatment),alpha=0.1)+
  geom_line(aes(col=Treatment),linejoin='round',size=0.8)+
  geom_point(aes(col=Treatment),size=3,shape=16)+
  geom_errorbar(data=quickleaf,aes(ymin=max(Avg+SE)-SED,ymax=max(Avg+SE)),col='grey30',
                position='dodge',width=0.1)+
  scale_color_brewer(palette='Set1')+
  labs(x='Month',y='Leaf biomass, kg/ha')+
  facet_wrap(~Method)+
  theme_test()
