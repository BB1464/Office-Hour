# Import SixSigma package
library(SixSigma)

# Design the experiment (2^3)
ExperimentDesign <- expand.grid(A = gl(2, 1, labels = c("-", "+")),
                                B = gl(2, 1, labels = c("-", "+")),
                                C = gl(2, 1, labels = c("-", "+")))

# Randomize the experiment
ExperimentDesign$ord <- sample(1:8, 8)
ExperimentDesign[order(ExperimentDesign$ord), ]

# Create replicates
ss.data.doe1 <- data.frame(repl = rep(1:2, each = 8),
                           rbind(ExperimentDesign))
ss.data.doe1

# Add responses
ss.data.doe1$response <- c(5.33, 6.99, 4.23, 6.61,
                           2.26, 5.75, 3.26, 6.24,
                           5.7, 7.71, 5.13, 6.76,
                           2.79, 4.57, 2.48, 6.18)
ss.data.doe1




#-------- PAU Student Randomization Scrpt for 4 Factors -------------------
#PAU Script
# Design the experiment (2^3)
ExperimentalDesign <- expand.grid(Genotypes = gl(4, 1, labels = c("a", "b",'c','d')),
                                REP = gl(3, 1, labels = c(1,2,3)),
                                Explant = gl(3, 1, labels = c("T1", "T2",'T3')),
                                Media_Types = gl(2, 1, labels = c('t','m')),
                                Seed_per_plant =  gl(10,1, labels = c(1,2,3,4,5,6,
                                                                      7,8,9,10)))

ExperimentalDesign$Number_of_leaf<-c('') #To add column
