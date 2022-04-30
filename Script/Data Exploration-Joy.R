library(tidyverse)
library(readxl)

dat <- read_excel(path = 'Data/Copy of Questionnaire.xlsx')




dat %>% ggplot(mapping = aes(x = factor(Age),y = `No of persons in household`))+geom_col()



dat2 <- dat %>% mutate(Age=case_when(Age%in%c(18:45)~'18-45',Age%in%c(46:70)~'46-70',TRUE~'47-89'))


View(dat2)

