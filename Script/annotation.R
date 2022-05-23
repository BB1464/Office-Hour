############################################################################
############################################################################
###                                                                      ###
###                        ANNOTATIONS IN GGPLOT2                        ###
###                                                                      ###
############################################################################
############################################################################

library(tidyverse)

ggplot(data = NULL,mapping = aes(x = c(1:4),y = c(1:4)))+
  annotate(geom = 'rect',xmin = 1,xmax = 7,ymin = 1,ymax = 7,fill='steelblue',col='lightgray')+
  theme_void()


# Second Approach

ggplot(data = mpg,aes(x = year,y = displ))+
  annotate(geom = 'rect',xmin = 2000,xmax = 2002,ymin = 2.9,ymax = 3.5,fill='gray')+
  stat_summary(geom = 'line',fun = 'mean')
