
# Load the required packages ----------------------------------------------

library(tidyverse)
library(readxl)


# Import the dataset ------------------------------------------------------

dat <- read_excel(path = here::here('Data/okon-data.xlsx'))



# Plot --------------------------------------------------------------------

# dat |>
#   mutate(Treatment=factor(Treatment,levels = c('Mock_24-h','ToMV_72-h','ToMV_8-h',' Mock_24-h','ToMV_24-h','Mock_48-h','ToMV_48-h','Mock_72-h'))) |>

dat <- dat |>
mutate(Treatment=factor(Treatment,levels = c('Mock_8-h','ToMV_8-h','Mock_24-h','ToMV_24-h','Mock_48-h','ToMV_48-h','Mock_72-h','ToMV_72-h')))


ggplot(data=dat,aes(x = Treatment,y = `SA  Content (ng/g  FW)`,fill=Treatment))+
stat_summary(geom = 'errorbar',width=0.5,fun.data = 'mean_se',position = position_dodge(.9),size=0.7)+stat_summary(geom = 'col',fun  = 'mean',position = position_dodge(.9))+
  scale_y_continuous(expand = expansion(mult = c(0,0.1)))+
  scale_fill_manual(values = c('Mock_8-h'='lightblue','ToMV_8-h'='pink','Mock_24-h'='lightblue','ToMV_24-h'='pink','Mock_48-h'='lightblue','ToMV_48-h'='pink','Mock_72-h'='lightblue','ToMV_72-h'='pink'))+
theme_bw()+
  guides(fill='none')+
theme(axis.text.x = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.title.x = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.text.y = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.title.y = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12))



# Save the Plot -----------------------------------------------------------


ggsave('Mock.png',path = here::here('Plot'),width = 9.3,height = 9.3,dpi = 320)


