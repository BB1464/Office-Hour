
library(SixSigma)
library(tidyverse)
library(readxl)




dat <- data.frame(incubation_time=c('7 days','14 days','21 days','35 days'),treatment=c('200g soil + 0ml POME','200g soil + 50ml POME','200g soil + 70ml POME','200g soil + 90ml POME'))




## First Design with Weeks

ExperimentalDesign <- expand.grid(Treatment = gl(4, 1, labels = Treatment <- c("200g soil + 0ml POME","200g soil + 50ml POME","200g soil + 70ml POME","200g soil + 90ml POME")),Incubation = gl(5, 1, labels = c("7 days", "14 days","21 days","28 days","35 days")))

p=ExperimentalDesign |>
  mutate(pH='') |>
  mutate(EC='') |>
  mutate(TN='') |>
  mutate(OC='') |>
  mutate(OM='') |>
  mutate(Ca='') |>
  mutate(Mg='') |>
  mutate(K='') |>
  mutate(Na='') |>
  mutate(H='') |>
  mutate(ECEC='')

## SAve the output

write.csv(x = p,file = here::here('Data/jeniva.csv'),row.names = FALSE)
