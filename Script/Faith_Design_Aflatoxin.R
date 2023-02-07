library(SixSigma)
library(tidyverse)
library(readxl)




dat <- data.frame(incubation_time=c('7 days','14 days','21 days','35 days'),treatment=c('200g soil + 0ml POME','200g soil + 50ml POME','200g soil + 70ml POME','200g soil + 90ml POME'))

Location <- c('dugbe','sango','oje','alesinloye','bodija')

Sample_Code <- c('FR-Dg 1','SD-Sg 1','PWD-Sg 1','PWD-OJ 1','PWD-AL 1','FR-AL 1','SD-AL 1','SD-OJ 1','FR-BO 1','PWD-BO 1','FR-OJ 1','FR-SG 1','PWD-DG 1','SD-BO 1','SD-DU 1')

Sample_Types <- c('fresh','sliced dried','powdered')

Replicate <- c('1','2')

Aflatoxin_Type <- c('AFB1','AFB2','AFG1','AFAFG2')




## First Design with Weeks

ExperimentalDesign <- expand.grid(Location = gl(5, 1, labels =  Location),Sample_Code = gl(15, 1, labels = Sample_Code),Sample_Types=gl(3,1,labels = Sample_Types),Replicate=gl(2,1,labels = Replicate),Aflatoxin_Type=gl(4,1,labels = Aflatoxin_Type))

p=ExperimentalDesign |>
  mutate(`Aflatoxin level`='')

## SAve the output

write.csv(x = p,file = here::here('Data/Faith_Aflatoxin.csv'),row.names = FALSE)
