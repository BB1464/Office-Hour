
iris |>
  ggplot(aes(x = Species,y = Sepal.Length,fill=Species))+
  stat_summary(geom = 'errorbar',fun.max = function(x)mean(x)+sd(x),fun.min = function(x)mean(x)-sd(x),width=.5,size=1)+
  geom_bar(stat = 'summary',width = .8)+
  scale_y_continuous(expand =c(0,0))+
  scale_x_discrete(expand = c(0,0))+
  theme_minimal()+
  geom_hline(yintercept = 0)+
  theme(legend.position = 'none',plot.background   = element_rect(fill = 'gray85',colour = 'gray85'),panel.grid = element_blank())+
  MetBrewer::scale_fill_met_d(name = 'Hokusai1')+
  theme(panel.grid.major.y = element_line(linetype = 'dotted',size = 1))




