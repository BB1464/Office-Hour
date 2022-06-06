library(SixSigma)



ExperimentalDesign <-
  expand.grid(
    Colour = gl(
      6,
      1,
      labels = c(
        "Black",
        "White",
        'Brown',
        'Black Brown',
        'White Brown',
        'White Black'
      )
    ),
    REP = gl(3, 1, labels = c(1, 2, 3, 4, 5)),
    Tem_Hud_Index = gl(2, 1, labels = c("High", "Low")),
    Temp = gl(92, 1, labels = c(1:92)),
    Humidity = gl(
      92,
      1,
      labels = c(1:92)),
      Time = gl(2, 1, labels = c('Morning', 'Afternoon')),
      Sex = gl(2, 1, labels = c('Male', 'Female')))






write.csv(x = ExperimentalDesign,'design2.csv',row.names = FALSE)









