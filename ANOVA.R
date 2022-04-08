# DOE Bioresearch for Analysis of Variance

library(doebioresearch)
library(tidyverse)

dat <- factorialdata

glimpse(dat)

dat <- dat %>% mutate(across(.cols = c(1:4),.fns = factor))

glimpse(dat)


model <- frbd3fact(data = dat[5:6],replicationvector = dat$Replication,fact.A = dat$Nitrogen,fact.B = dat$Phosphorus,fact.C = dat$Potassium,Multiple.comparison.test = 3)


model
