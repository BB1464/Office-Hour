############################################################################
############################################################################
###                                                                      ###
###                             GGXMEAN DEMO                             ###
###                                                                      ###
############################################################################
############################################################################




library(tidyverse)
library(ggxmean)
palmerpenguins::penguins %>%
  ggplot() +
  aes(x = bill_length_mm) +
  aes(y = flipper_length_mm) +
  geom_point() +
  ggxmean::geom_x_mean() +
  ggxmean::geom_y_mean() +
  ggxmean:::geom_xdiff() +
  ggxmean:::geom_ydiff() +
  ggxmean:::geom_x1sd(linetype = "dashed") +
  ggxmean:::geom_y1sd(linetype = "dashed") +
  ggxmean:::geom_diffsmultiplied() +
  ggxmean:::geom_xydiffsmean(alpha = 1) +
  ggxmean:::geom_rsq1() +
  ggxmean:::geom_corrlabel() +
  facet_wrap(facets = vars(species))
