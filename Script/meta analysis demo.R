

library(metafor)
library(tidyverse)

dat <- KenSyn::organic


mc <- escalc(measure = 'SMD',yi =Yield_conv,vi = Yield_org,data=dat,append = TRUE,)



mod <- rma(yi = yi,vi = vi,data = mc)

forest(x=mod)

AC <- mc %>% filter(Study%in%c('study1','study2','study3','study4','study5'))

mod1 <- rma(yi = yi,vi = vi,data = AC)

forest(mod1)


