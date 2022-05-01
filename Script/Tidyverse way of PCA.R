
############################################################################
############################################################################
###                                                                      ###
###                   PCA USING THE TIDYVERSE APPROACH                   ###
###                                                                      ###
############################################################################
############################################################################



library(tidyverse)
library(broom)
library(MASS)
library(cowplot)

data(iris)


pca <- iris %>% dplyr::select(where(is.numeric)) %>% 
  scale() %>% 
  prcomp()


# PCA

 pca %>% 
  augment(iris) %>% 
   ggplot(mapping = aes(.fittedPC1,.fittedPC2,col=Species))+
   geom_point(size=1.5)+
   scale_color_manual(values = c(setosa='#D55E00',versicolor='#0072b2',virginica='red'))+
   theme_half_open(12)+
   background_grid()
 
 # extract rotation matrix
 pca %>% 
   tidy(matrix='rotation')
 
 
 # Define arrow style for plotting
 
 arrow_style <- arrow(angle = 20,ends = 'first',type = 'closed')

 
 # Plot the rotation matrix
 
 pca %>% 
   tidy(matrix='rotation') %>% 
   pivot_wider(names_from = 'PC',values_from = 'value',names_prefix = 'PC') %>% ggplot(mapping = aes(x = PC1,y = PC2))+
   geom_segment(xend=0,yend=0,arrow=arrow_style)+
   geom_text(mapping = aes(label=column),hjust=1,nudge_x = -0.02,color='#904C2F')+coord_fixed()+
   theme_half_open()+background_grid()

 # Look at the varaiance explained by each PC
 
 pca %>% 
   tidy(matrix='eigenvalues')

 # Visualize the scree plot
 
 pca %>% 
   tidy(matrix='eigenvalues') %>% 
   ggplot(mapping = aes(x = PC,y = percent))+
   geom_col(fill='#56B4E9',alpha=0.8)+
   scale_x_continuous(breaks = c(1:4))+
   scale_y_continuous(labels=scales::percent_format(),expand = expansion(mult = c(0,0.01)))+
   theme_minimal_hgrid(12)
 