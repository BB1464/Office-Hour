
###########################################################################
###########################################################################
###                                                                     ###
###                           FILTER FUNCTION                           ###
###                                                                     ###
###########################################################################
###########################################################################

library(tidyverse)

 msleep %>%
  filter(if_all(.cols = everything(),.fns = ~!is.na(.x)))


# Alternatively

 msleep %>%
   filter(across(.cols = everything(),.fns = ~!is.na(.x)))



 # Rowwise function review
 # Sum to first NA
 ID <- c("A", "B", "C", "D", "E")
 Q1 <- c(NA, 1, 2, NA, 4)
 Q2 <- c(5, NA, 7, 8, 9)
 Q3 <- c(0, 1, NA, 3, 4)
 df <- data.frame(ID, Q1, Q2, Q3)

 sum_to_first_na <- function(x) {
   ss <- 0
   i <- 1
   repeat {
     if (is.na(x[i])) break
     ss <- ss + x[i]
     i <- i + 1
   }
   return(ss)
 }

 df$sum <- apply(df[, 2:4], 1, sum_to_first_na)

  df

