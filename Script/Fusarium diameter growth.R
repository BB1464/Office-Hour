
############################################################################
############################################################################
###                                                                      ###
###                      DATA ANALYSIS                                   ###
###                                                                      ###
############################################################################
############################################################################



############################################################################
############################################################################
###                                                                      ###
###                          IMPORT THE DATASET                          ###
###                                                                      ###
############################################################################
############################################################################

Faith<-read.csv('Data/Fusarium radial growth.csv')

############################################################################
############################################################################
###                                                                      ###
###                      LOAD THE NECESSARY LIBRARY                      ###
###                                                                      ###
############################################################################
############################################################################

library(tidyverse)

############################################################################
############################################################################
###                                                                      ###
###                   PREPARE THE DATASET FOR ANALYSIS                   ###
###                                                                      ###
############################################################################
############################################################################

glimpse(Faith)

Faith <- Faith %>% mutate(across(.cols = c(1:3),.fns = factor))


############################################################################
###                                                                      ###
###                   LINE PLOT                                          ###
###                                                                      ###
############################################################################
############################################################################

theme_set(theme_test())

Faith %>% group_by(Hours, Isolate.Code) %>%
 summarise(Diameter.growth = mean(Diameter.growth)) %>%
  ggplot(aes(Hours, Diameter.growth, group = Isolate.Code, col = Isolate.Code)) +
  geom_line(size = 1) + scale_color_manual(name = 'Isolate codes', values =  c("#EEDFCC", "#76EEC6", "#00008B", "#FF8C00", "#006400", "#EE1289", "#528B8B", "#8B7500", "#0F0F0F", "#F0E68C", "#00FF00", "#8DB6CD", "#FF4500", "#EE0000", "#43CD80", "#C0FF3E", "#00F5FF", "#CDC9C9", "#545454", "#0000FF", "#8B008B", "#FFFF00", "#D2691E", "#8B7500", "#B452CD")) +
  theme(
    text = element_text(family = 'serif', face = 'bold'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(),legend.text = element_text(size = 6)
  ) +
  labs(y = 'Diameter growth')+
  guides(color=guide_legend(ncol = 1, byrow=FALSE))

# Save the plot
ggsave('Diameter1.tiff',path = 'Plot',width = 12,height = 9,dpi = 300)



# Alternatively Option Two

# install.packages("devtools") # Uncomment this line and install the package if you don't have it.

# devtools::install_github("mtennekes/cols4all")



# Annonymous function to check if the packages are inmstalled on your system.

if(!requireNamespace(c('cols4all','devtools'))){
  devtools::install_github("mtennekes/cols4all")
}



library(cols4all)


Cols <- cols4all::c4a(palette = 'glasbey',n = 25)



Faith %>% group_by(Hours, Isolate.Code) %>%
  summarise(Diameter.growth = mean(Diameter.growth)) %>%
  ggplot(aes(Hours, Diameter.growth, group = Isolate.Code, col = Isolate.Code)) +
  geom_line(size = 1) + scale_color_manual(name = 'Isolate codes', values = Cols)+
  theme(
    text = element_text(family = 'serif', face = 'bold'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line()
  ) +
  labs(y = 'Diameter growth')+
  guides(color=guide_legend(ncol = 1, byrow=FALSE))





# Save the plot
ggsave('Diameter2.tiff',path = 'Plot',width = 12,height = 9,dpi = 300)
