library(tidyverse)
library(readxl)

dat <- read_excel(path = 'Data/Copy of Questionnaire.xlsx')


dat %>% mutate(Age=case_when(Age%in%c(18:41)~'18-41',Age%in%c(42:56)~'42-56',Age%in%c(57:63)~'57-63',TRUE~'65-89')) %>%
  mutate(Gender=fct_recode(Gender,'Male'='M','Female'='F')) %>%
  ggplot(mapping = aes(Age,`No of persons in household`,fill=`Major Source of Income`))+geom_col()+facet_grid(~Gender)+scale_fill_manual(values = c('red','blue','green','pink','tomato',"seagreen3",'gray','yellow','cyan','brown'))+
  labs(x='Age Category of the Respondent',y='Household Size')+
  theme_minimal()+
  theme(strip.background = element_rect(fill = NA,colour = 'black'),text = element_text(family = 'serif',face = 'bold',size = 16),axis.line = element_line(),panel.background = element_rect())


# Plot of HouseHold Size
ggsave('Household_size.png',width = 12,height = 9,dpi = 400,path = 'Plot')



# Sheet 2
dat2 <- read_excel(path = 'Data/Copy of Questionnaire.xlsx',sheet = "Ethnobotany and Uses")

# Inner_Join by QID
dat3 <- dat %>% select(QID,Age,Gender)

dat4 <- inner_join(dat3,dat2,by='QID')



dat4 %>% mutate(Age=case_when(Age%in%c(18:41)~'18-41',Age%in%c(42:56)~'42-56',Age%in%c(57:63)~'57-63',TRUE~'65-89')) %>%
  mutate(Gender=fct_recode(Gender,'Male'='M','Female'='F')) %>%
  mutate(History=ifelse(`History of Plant use`==1,'Ancestral',ifelse(`History of Plant use`==2,'Introduced unknown','Introduced Known'))) %>%
  mutate(Frequency=ifelse(`Frequency of cooking`==2,'Weekly',ifelse(`Frequency of cooking`==3,'Occasional','Occasional'))) %>%
  mutate(`Local name of variety`=case_when(`Local name of variety`%in%c('Kokoghana','kokoghana')~'Dasheen',TRUE~'Eddoe')) %>%
mutate(`Palatability of cooked corm`=ifelse(`Palatability of cooked corm`==2,'Acceptable','Good')) %>%
ggplot(mapping = aes(x=`Local name of variety`,fill=State))+geom_bar()+facet_grid(~`Palatability of cooked corm`)+labs(x='Variety')+
  scale_fill_manual(values = c('red','blue','green','tomato','cyan','gray50','seagreen3'))+
theme_minimal()+
theme(panel.background = element_rect(),strip.background.x = element_rect(fill = NA,colour = 'black'),text = element_text(family = 'serif',size = 18,face = 'bold'),legend.position = c(0.1,0.75))+
scale_y_continuous(limits = c(0,60),breaks = seq(0,60,10))


# Save the plot
ggsave('Palatability.png',width = 12,height = 9,dpi = 400,path = 'Plot')













