
library(tidyverse)
library(readxl)


dat <- read_excel('Data/Socio-economic analysis FF.xlsx') |>
  janitor::clean_names()

head(dat)
dat <- dat |> mutate(respondent_age=factor(respondent_age))

iris |>
  mutate(Spec=case_when(Species%in%c('setosa','virginica')~'0-3',TRUE~'4-5'))


# Respondent Age by Taro Field
dat1 <- dat |>
  mutate(AGE=case_when(respondent_age%in%c('23','30','32','35','40')~'20-40',respondent_age%in%c('45','48','50','57','58')~'41-60',TRUE~'61-81'))

ggplot(data = dat1,aes(x =AGE,y = taro_field_area_m2,fill=factor(gender)))+
  stat_summary(geom = 'col',fun = 'mean',position = 'dodge')+
  labs(x='Respondent Age',y='Taro Field Area (m2)',fill='Gender')+
  scale_fill_brewer(palette = 'Set1')+
scale_y_continuous(expand = c(0,0))+
  theme_minimal()+
  theme(axis.line = element_line())

ggsave('Taro_Field.png',width = 10,height = 8,dpi = 400,path = here::here('Plot'))


# Respondent Age by Household Size
ggplot(data = dat1,aes(x =AGE,y = household_size,fill=factor(gender)))+
  stat_summary(geom = 'col',fun = 'mean',position = 'dodge')+
  labs(x='Respondent Age',y='Household Size',fill='Gender')+
  scale_fill_brewer(palette = 'Set1')+
  scale_y_continuous(expand = c(0,0))+
  theme_minimal()+
  theme(axis.line = element_line())


ggsave('Household.png',width = 10,height = 8,dpi = 400,path = here::here('Plot'))



