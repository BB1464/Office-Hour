
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

dat <- read_csv(file = 'Data/dat.csv')



dat1 <- dat1 %>% mutate(across(.cols = c(2:7),.fns = factor))


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






write.csv(x = ExperimentalDesign,'design.csv',row.names = FALSE)









