

library(tidyverse)
library(ggpmisc)
library(ggtext)


ggplot(data = iris,mapping = aes(x = Sepal.Length,y = Sepal.Width))+
geom_smooth(se = FALSE,formula = y~x)+
stat_poly_eq(mapping = aes(label=paste(..eq.label..,..adj.rr.label..,..p.value.label..,sep='~~~')),parse = TRUE,coef.digits = 3,p.digits = 3,small.p = TRUE)+theme_light()
  

