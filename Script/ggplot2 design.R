
############################################################################
############################################################################
###                                                                      ###
###                            GGPLOT2 DESIGN                            ###
###                                                                      ###
############################################################################
############################################################################




library(tidyverse)
library(patchwork)

p1 <- ggplot(mtcars) +
  geom_point(aes(mpg, disp)) +
  ggtitle('Plot 1')

p2 <- ggplot(mtcars) +
  geom_boxplot(aes(gear, disp, group = gear)) +
  ggtitle('Plot 2')

p3 <- ggplot(mtcars) +
  geom_point(aes(hp, wt, colour = mpg)) +
  ggtitle('Plot 3')

p4 <- ggplot(mtcars) +
  geom_bar(aes(gear)) +
  facet_wrap(~cyl) +
  ggtitle('Plot 4')

design <- "
 111222
 334445
"

p1 + p2 + p3 + p4 + guide_area() +
  plot_layout(design = design, guides = "collect")
