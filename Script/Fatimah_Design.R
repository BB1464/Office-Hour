library(agricolae)

trt1 <- c('Open Net','Single Net','Triple Net')

trt2 <- c('High','Medium','Low')

design.split(trt1 = trt1,trt2 = trt2,r = 4,randomization = TRUE)





library(SixSigma)



ExperimentalDesign <- expand.grid(Super_Tree = gl(5, 1, labels = c(1,2,3,4,5)),
                                  REP = gl(4, 1, labels = c(1,2,3,4)),
                                  Net = gl(3, 1, labels = c("Triple", "Single",'Open')),
                                  Water_Regime = gl(3, 1, labels = c('High','Low','Medium')))



