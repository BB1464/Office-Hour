
############################################################################
############################################################################
###                                                                      ###
###                         ANALYSIS OF VARIANCE                         ###
###                                                                      ###
############################################################################
############################################################################


setwd(dir = '~/../Desktop')

library(tidyverse)
library(doebioresearch)
library(readxl)
library(mice)
library(VIM)
library(tidyverse)
library(agricolae)
library(janitor)


# New dataset
data <- read_csv('design.csv') %>% clean_names()


data <- read_csv(file = 'Data/design to be send.csv') %>%
  clean_names()



data <- data %>% mutate(across(.cols = c(1:4),.fns = factor))



# Missing data
summary(data)
md.pattern(data)#This display the missing data pattern


#Missing data imputation
imputed_Data<-mice(data,m=5,maxit=50,method='pmm', seed = 500)
summary(imputed_Data)

# #Check imputed values
# imputed_Data$imp$Radicle_Length
# imputed_Data$imp$SEEDLING_LGTH_IN_CM
# imputed_Data$imp$NO._OF_LEAVES
# imputed_Data$imp$NO._OF_ROOTS


#Get complete data (2nd out of 5)
completeData<-complete(imputed_Data,2)
completeData


# Demo Mocel
model <- doebioresearch::crd(data = completeData[5:11],trt.vector = completeData$accesion,MultipleComparisonTest = 2)


model



#################################################################
##                            Model                            ##
#################################################################

model <- crd(data = dat[5:11],trt.vector = dat$Accesion,MultipleComparisonTest = 3)


model





############################################################################
############################################################################
###                                                                      ###
###                         DESIGN OF EXPERIMENT                         ###
###                                                                      ###
############################################################################
############################################################################


library(SixSigma)



ExperimentalDesign <- expand.grid(State = gl(3, 1, labels = c("Oueme", "Zou",'Ketou')),
                                  REP = gl(3, 1, labels = c(1,2,3)),
                                  Villages = gl(4, 1, labels = c("Hounvigue", "Adjohoun",'Zounnou','Sodji')),
                                  Accession = gl(30, 1, labels = c('A1','A2','A3','A4','A5','A6','A7','A8','A9','A10','A11','A12','A13','A14','A15','A16','A17','A18','A19','A20','A21','A22','A23','A24','A25','A26','A27','A28','A29','A30')))



design <- expand.grid(TRT1=gl(3,1,labels = c(0,1,2)),
                      TRT2=gl(2,1,labels = c(0,1)),
                      TRT3=gl(4,1,labels = c(0,1,2,3)),
                      Rep=gl(3,1,labels = c(1,2,3)))









ExperimentalDesign <- expand.grid(State = gl(3, 1, labels = c("Oueme", "Zou",'Ketou')),Villages = gl(4, 1, labels = c("Hounvigue", "Adjohoun",'Zounnou','Sodji')),
                                  Accession = gl(30, 1, labels = c('A1','A2','A3','A4','A5','A6','A7','A8','A9','A10','A11','A12','A13','A14','A15','A16','A17','A18','A19','A20','A21','A22','A23','A24','A25','A26','A27','A28','A29','A30')))






write.csv(x = ExperimentalDesign,'design2.csv',row.names = FALSE)









