# DOE Bioresearch for Analysis of Variance

library(doebioresearch)
library(tidyverse)

dat <- factorialdata

glimpse(dat)

dat <- dat %>% mutate(across(.cols = c(1:4),.fns = factor))

glimpse(dat)


model <- frbd3fact(data = dat[5:6],replicationvector = dat$Replication,fact.A = dat$Nitrogen,fact.B = dat$Phosphorus,fact.C = dat$Potassium,Multiple.comparison.test = 3)


model


# CRD Analysis
model <- crd(data = dat[5:6],trt.vector = dat$Nitrogen,MultipleComparisonTest = 2)

model


mod <- fcrd2fact(data = dat[5:6],fact.A = dat$Nitrogen,fact.B = dat$Phosphorus,Multiple.comparison.test = 2)

mod
