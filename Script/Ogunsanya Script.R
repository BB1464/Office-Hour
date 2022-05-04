############################################################################
############################################################################
###                                                                      ###
###                       FIELD DATA VISUALIZATION                       ###
###                                                                      ###
############################################################################
############################################################################


library(tidyverse)
library(readxl)
library(cowplot)

data1 <- read_excel('Data/Copy of Field data Mature trait.xlsx')


data1 <- data1 %>% mutate(across(.cols = c(1:4),.fns = factor))


# Stem Colour
p1 <- ggplot(data = data1,mapping = aes(x = Species,y = `Stem Colour`,fill=Category...4))+geom_col()


Leaf Margin colours
p2 <- ggplot(data = data1,mapping = aes(x = Species,y = `Leaf Margin Colour`,fill=Category...8))+geom_col()



plot_grid(p1,p2,nrow = 2,ncol = 1,rel_widths = c(1,1))
