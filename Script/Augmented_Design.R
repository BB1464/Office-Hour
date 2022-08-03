
# Design of Augmented Block in R

library(agricolae)
library(tidyverse)

# Year One

check <- c('Check1','Check2','Check3')

Treatment <- c(paste('G',sep = '',1:54))


Design <- design.dau(trt1 = check,trt2 = Treatment,r = 5,seed = 400,randomization = TRUE)


Plan <- Design$book


write_csv(x = Plan,file = 'Data/Year_One_Plan.csv')


# Year Two


check <- c('Check1','Check2')

Treatment <- c(paste('G',sep = '',1:65))


Design <- design.dau(trt1 = check,trt2 = Treatment,r = 5,seed = 400,randomization = TRUE)


Plan <- Design$book


write_csv(x = Plan,file = 'Data/Year_Two_Plan.csv')

# Combined

check <- c('Check1','Check2','Check3','Check4','Check5')

Treatment <- c(paste('G',sep = '',1:109))


Design <- design.dau(trt1 = check,trt2 = Treatment,r = 10,seed = 400,randomization = TRUE)


Plan <- Design$book


write_csv(x = Plan,file = 'Data/Combine_Plan_Plan.csv')

