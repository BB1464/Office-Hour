###########################################################################
###########################################################################
###                                                                     ###
###                     EXPLORATORY DATA ANALYSIS 2                     ###
###                                                                     ###
###########################################################################
###########################################################################


library(tidyverse)
library(readxl)


data <- read_excel('Data/Questionnaire_Market.xlsx',sheet = 'Market')



ggplot(data = data,mapping = aes(x = Gender,y = `No of persons in household`,fill=factor(`Number of income source`)))+geom_col(position =position_dodge ())+scale_fill_brewer(name='Number of Income Source',palette = 'Set1')+
  theme_classic()+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(label=c('Female','Male'))+
  theme(text = element_text(family = 'serif',face = 'bold',size = 14),axis.line = element_line(size = 1,color = 'black'),axis.ticks  = element_line(size = 1))

# Export the graph
ggsave('Market1.png',width = 10,height = 7,dpi = 400,path = here::here('Plot'))


# Second Plot

data <- data %>%
  mutate(Lang=fct_recode(`Language of the taro name`,'Ibibio'='Efik'))





 ggplot(data = data,mapping = aes(Market,fill=`Name of variety  sold`))+geom_bar()+facet_grid(~Lang)+
   scale_fill_brewer(palette = 'Set1')+theme_test()+
   theme(axis.text.x = element_text(angle = 45,family = 'serif',face = 'bold',hjust = 1),strip.background.x = element_rect(fill = NA,colour = 'black'))+
   scale_y_continuous(limits = c(0,8),expand = c(0,0))

 # Export the graph
 ggsave('Market2.png',width = 10,height = 7,dpi = 400,path = here::here('Plot'))



# THird Graph
ggplot(data = data,mapping = aes(x = Market,y = `Volume sold per market day in Kg`,fill=`Demand for taro`))+geom_col()+scale_fill_brewer(palette = 'Set1')+
labs(y='Volume sold per market day (Kg)')+scale_y_continuous(expand = c(0,0))+
theme(text = element_text(family = 'serif',face = 'bold',size = 12),axis.line = element_line(size = 1,colour = 'black'))+
  theme_classic()


# Export the graph
ggsave('Market3.png',width = 10,height = 7,dpi = 400,path = here::here('Plot'))
