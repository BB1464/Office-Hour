

library(tidyverse)

typo_dframe <- tribble(
  ~pre_test, ~post_test,
  "goud",      "good",
  "medium",    "good",
  "metium",     "metium",
  "bad",        "goud"
)



# Fix the typo
typo_dframe |>
  mutate(across(.cols = everything(),
                .fns = ~case_when(str_detect(string = .,pattern = 'goud')~str_replace(string = .,pattern = 'goud',replacement = 'good'),str_detect(string = .,pattern = 'metium')~str_replace(string = .,pattern = 'metium',replacement = 'medium'),TRUE~.)))





# Demo Two Typo

dat <- data.frame(Sex=c('male','female','gender','male','female'))


dat |> mutate(across(.cols = everything(),.fns = ~case_when(str_detect(string = .,pattern = 'gender')~str_replace(string = .,pattern = 'gender',replacement = 'female'),TRUE~.)))
