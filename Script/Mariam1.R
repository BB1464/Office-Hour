
# Import the Packages ------------------------------------------

library(tidyverse)
library(agricolae)
library(readxl)


# Import the dataset ----------------------------------------

dat <- read_excel(path = here::here('Data/mariam.xlsx'),sheet = 1)

## Convert Characters to Factor
dat <- dat |> mutate(across(.cols = c(1:5),.fns = factor))

## First model

for (i in 6:ncol(dat)) {

print(paste('----',names(dat[i]),'----'))

mod <- lm(dat[[i]]~REP+Location*Accession*Year*Weeks,data=dat)

print(anova(mod))
print(cv.model(mod))

lala=HSD.test(y = mod,trt = 'Location',console = TRUE)
lala=HSD.test(y = mod,trt = 'Accession',console = TRUE)
lala=HSD.test(y = mod,trt = 'Year',console = TRUE)
lala=HSD.test(y = mod,trt = 'Weeks',console = TRUE)
lala=HSD.test(y = mod,trt = c('Location','Accession'),console = TRUE)
lala=HSD.test(y = mod,trt = c('Location','Year'),console = TRUE)
lala=HSD.test(y = mod,trt = c('Location','Weeks'),console = TRUE)
lala=HSD.test(y = mod,trt = c('Accession','Year'),console = TRUE)
lala=HSD.test(y = mod,trt = c('Accession','Weeks'),console = TRUE)
lala=HSD.test(y = mod,trt = c('Year','Weeks'),console = TRUE)
lala=HSD.test(y = mod,trt = c('Location','Accession','Year'),console = TRUE)
lala=HSD.test(y = mod,trt = c('Location','Year','Weeks'),console = TRUE)

lala=HSD.test(y = mod,trt = c('Accession','Year','Weeks'),console = TRUE)

lala=HSD.test(y = mod,trt = c('Location','Accession','Year','Weeks'),console = TRUE)

print(lala)

}
