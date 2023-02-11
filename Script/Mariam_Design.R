
library(SixSigma)
library(tidyverse)
library(readxl)


dat <- read_excel(path = here::here('Data/mariam.xlsx'),sheet = 'dat1')





## First Design with Weeks

ExperimentalDesign <- expand.grid(Accession = gl(22, 1, labels = Accession <- c("TKg-23","TKg-24","TKg-25","TKg-26","TKg-27","TKg-28","TKg-29","TKg-30","TKg-31","TKg-32","TKg-33","TKg-34","TKg-44","TKg-35","TKg-36","TKg-37","TKg-38","TKg-39","TKg-40","TKg-41","TKg-42","TKg-43")),REP = gl(3, 1, labels = c(1,2,3)),
Location = gl(2, 1, labels = c("Ikenne", "Ibadan")),
Year = gl(2, 1, labels = c('2019','2020')),
Weeks =  gl(2,1, labels = c(5,10)))


## Second Design
ExperimentalDesign2 <- expand.grid(Accession = gl(22, 1, labels = Accession <- c("TKg-23","TKg-24","TKg-25","TKg-26","TKg-27","TKg-28","TKg-29","TKg-30","TKg-31","TKg-32","TKg-33","TKg-34","TKg-44","TKg-35","TKg-36","TKg-37","TKg-38","TKg-39","TKg-40","TKg-41","TKg-42","TKg-43")),REP = gl(3, 1, labels = c(1,2,3)),
                                  Location = gl(2, 1, labels = c("Ikenne", "Ibadan")),
                                  Year = gl(2, 1, labels = c('2019','2020')))



## Save the excel sheet

write.csv(x = ExperimentalDesign,row.names = FALSE,file = here::here('Data/Mariam_Design.csv'))


write.csv(x = ExperimentalDesign,row.names = FALSE,file = here::here('Data/Mariam_Design2.csv'))
