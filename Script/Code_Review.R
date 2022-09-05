library(tidyverse)

df<-tibble(
  red = c("1", "2","4","6","4"),
  score=c(22,43,73,66,35),
  group=c(1,2,3,4,5),
  response=c("yes","yes","yes",NA,NA),
  question=c(NA, NA, "think", "think", "think"),
  blue = c("2","4", "1","6","3"),
  green=c(8,4,6,5,3),
  pink=c(5,3,4,5,3),
  id=c(1,2,3,4,5),
  statement=c("prefer not to answer", NA,NA,"prefer not to answer","prefer not to answer")
)



df |>
mutate(response=case_when(str_detect(response,'yes')~1,TRUE~0)) |>
  mutate(question=case_when(str_detect(question,'think')~1,TRUE~0))




# Review this Code

tibble(
  red = c("1", "2","4","6","4"),
  score=c(22,43,73,66,35),
  group=c(1,2,3,4,5),
  response=c("yes","yes","yes",NA,NA),
  question=c(NA, NA, "think", "think", "think"),
  blue = c("2","4", "1","6","3"),
  green=c(8,4,6,5,3),
  pink=c(5,3,4,5,3),
  id=c(1,2,3,4,5),
  statement=c("prefer not to answer", NA,NA,"prefer not to answer","prefer not to answer")
) |>
  # select(question, statement) |>
  mutate(across(c(question, response), function(x) {

    case_when(
      is.na(x) & !is.na(statement) ~ "0",
      # Define the logic here
    )
  }
  ))









