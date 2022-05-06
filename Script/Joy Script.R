###########################################################################
###########################################################################
###                                                                     ###
###                        ANALYSIS PIPELINE JOY                        ###
###                                                                     ###
###########################################################################
###########################################################################


library(tidyverse)
library(readxl)


data <- read_excel('Data/Copy of Questionnaire_Market.xlsx',sheet = 'Market')


ggplot(data = data,aes(x = Market,fill=`Source of taro sold`))+geom_bar()+
  scale_fill_brewer(palette = 'Dark2')+
  theme(axis.text.x = element_text(angle = 45,family = 'serif',hjust = 1))+
theme_classic()+
  scale_y_continuous(expand = c(0,0))


# Save the graph
ggsave(path = here::here('Plot'),width = 10,height = 8,dpi = 400,'Market4.png')




# Farm In-ventory


data <- read_excel('Data/Copy of Questionnaire_Market.xlsx',sheet = 'Farm inventory')


#ggplot(data = data,mapping = aes(Gender,fill=`Agro-ecology`))+geom_bar()




data <- data %>% mutate(Gender=fct_recode(Gender,'Male'='M','Female'='F')) %>%
mutate(Gender=factor(Gender,levels = c('Male','Female')))

ggplot(data = data,mapping = aes(x = factor(`Number of Intercrops`),y = `percentage of field occupied by taro`))+geom_col(position = position_dodge())+facet_grid(~Gender)+  labs(x='Number of Intercrops',y='Percentage of Field Occupied by Taro (%)')+
  theme_test()+
  theme(strip.background.x = element_rect(fill = NA))


# Save the graph
ggsave(path = here::here('Plot'),width = 10,height = 8,dpi = 400,'Inventory1.png')




ggplot(data,aes(`Mean severity of TLB plants`,`TLB Incidence (%)`))+geom_col(position = position_dodge())+facet_grid(~`Agro-ecology`)



# Save the graph
ggsave(path = here::here('Plot'),width = 10,height = 8,dpi = 400,'Inventory2.png')



# The Last Sheet


data <- read_excel('Data/Copy of Questionnaire_Market.xlsx',sheet = 'Ethnobotany and Uses')

data$`Cropping system adopted` <- factor(data$`Cropping system adopted`)


data <- data %>% mutate(`Cropping system adopted`=fct_recode(`Cropping system adopted`,'Sole cropping'='1','Intercropping'='2'))

ggplot(data = data,mapping = aes(Community,`popularity among farmers and the people`,fill=factor(`Cropping system adopted`)))+geom_col()+
  theme_classic()+
  scale_fill_brewer(name='',palette = 'Dark2')+
  theme(axis.text.x = element_text(family = 'serif',hjust = 1,angle = 45))



# Save the graph
ggsave(path = here::here('Plot'),width = 10,height = 8,dpi = 400,'Ethnobotany1.png')
