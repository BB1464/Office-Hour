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




# Ethnobotany
data <- read_excel('Data/Copy of Questionnaire_Market.xlsx',sheet = 'Ethnobotany and Uses')

data$`Frequency of cooking` <- factor(data$`Frequency of cooking`)
data$`Main cooking method` <- factor(data$`Main cooking method`)
data$`Uses of the plant` <- factor(data$`Uses of the plant`)

data1 <- data %>% mutate(`Main cooking method`=fct_recode(`Main cooking method`,'Boiling'='1','Local Specialties'='4')) %>% mutate(`Frequency of cooking`=fct_recode(`Frequency of cooking`,'Weekly'='2','Occasional'='3')) %>%  mutate(`Uses of the plant`=fct_recode(`Uses of the plant`,'Food'='1','Food and Animal Feed'='2','Food and Export Commodity'='3'))




ggplot(data = data1,aes(x = State,fill=`Uses of the plant`))+
  geom_bar()+
  facet_grid(`Main cooking method`~`Frequency of cooking`)+
  theme_test()+scale_fill_brewer(palette = 'Set1')+
  theme(strip.background = element_rect(fill = NA),axis.text.x = element_text(angle = 45,hjust = 1))


ggsave(path=here::here('Plot'),'Ethno2.png',width = 10,height = 7,dpi = 400)



# Ethno 3
data$`History of Plant use` <- factor(data$`History of Plant use`)


data1 <- data %>%
  mutate(`History of Plant use`=fct_recode(`History of Plant use`,'Ancestral/Indigenous'='1','Introduced_Unknown Distant Past'='2','Introduced_Time of Introduction Known'='3'))


ggplot(data=data1,aes(x = Community,fill=`History of Plant use`))+geom_bar(position = position_dodge())+
  theme_test()+
  scale_fill_brewer(palette = 'Set1')+
  theme(axis.text.x = element_text(hjust = 1,angle = 45))


ggsave(path=here::here('Plot'),'Ethno3.png',width = 10,height = 7,dpi = 400)


# Taro Production and Contraint

data <- read_excel('Data/Copy of Questionnaire_Market.xlsx',sheet = 'Taro pdn n constraints')


# Taro Constraint
data$`Affected by TLB` <- factor(data$`Affected by TLB`)
data$`Taro Production Trend` <- factor(data$`Taro Production Trend`)


data1 <- data %>% mutate(`Affected by TLB`=fct_recode(`Affected by TLB`,'Presence of TLB'='1','Absence of TLB'='2')) %>%
  mutate(`Taro Production Trend`=fct_recode(`Taro Production Trend`,'Decreasing'='1','About the Same'='2','Increasing'='3'))



ggplot(data1,aes(x=`Years of growing taro`,fill=`Taro Production Trend`))+geom_bar()+facet_grid(~`Affected by TLB`)+
  theme(strip.background.x = element_rect(fill = 'white',colour = 'black'))+
  theme_test()+
scale_fill_brewer(palette = 'Set1')


ggsave(path=here::here('Plot'),'Production1.png',width = 10,height = 7,dpi = 400)


# Production Trend

ggplot(data,aes(x = Year,y = Production,group=1))+stat_summary(geom = 'line',size=1)+labs(y='Taro Production (kg/yr)')+theme_classic()

ggsave(path=here::here('Plot'),'Production2.png',width = 10,height = 7,dpi = 400)



