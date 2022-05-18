
ExperimentalDesign <- expand.grid(Phases = gl(2, 1, labels = c("Deplition", "Replition")),Diet = gl(6, 1, labels = c("Commercial", "Commercial_Phosphorus",'RBBGL','BBBGL','CBBGL','CBBGLF')),
                                  Rep = gl(7, 1, labels = c(1,2,3,4,5,6,7)))






write.csv(x = ExperimentalDesign,here::here('Data/Bio-availability.csv'),row.names = FALSE)


