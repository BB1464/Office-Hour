library(tidyverse)


dat <- data.frame(x1 = c("PET", "Dog", "Cat", "Fish",
                         "STATE", "CA", "NY", "NJ",
                         "LANGUAGE", "English", "Spanish"),
                  x2 = c(NA_character_, "60%", "30%", "10%",
                         NA_character_, "20%", "50%", "30%",
                         NA_character_, "40%", "60%"))

cleaned <-
  dat %>%
  mutate(category = if_else(condition = is.na(x2), true = x1, false = NA_character_), .before = x1) %>%
  fill(category) %>%
  filter(!is.na(x2)) %>%
  rename(item = x1, pc = x2) %>%
  mutate(pc_num = parse_number(pc))
