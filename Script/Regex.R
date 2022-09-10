library(tidyverse)

x <- tibble(address_full = "PO Box 233, Houston, TX ,77019-3333")

capture_groups <- "(.+?)\\s*,\\s*(\\d{5}-\\d{4})"

x %>%
  mutate(address = str_replace(address_full, capture_groups, "\\1"),
         zip     = str_replace(address_full, capture_groups, "\\2"))


# Second Solution

x <- tibble(address_full = "PO Box 233, Houston, TX ,77019-3333")

pattern = "(.*) ,(\\d{5}-\\d{4})"

x <- x %>%
  tidyr::extract(address_full, into = c("address", "zip"), regex = pattern)


# Extract string at a given pattern
stringr::str_split("xyz;abc", ";")


stringr::str_split(string = "xyz;abc",pattern = ";")





data <- tibble(Height=c(4,6,7,NA,NA),Weight=c(NA,NA,3,4,6))


library(dplyr)
library(tidyr)

data %>%
  mutate(
across(.cols = everything(), .fns = ~replace_na(.x, sd(.x,na.rm=TRUE)))
  )






library(dplyr)
library(tidyr)

dat <- data.frame('date' = Sys.time(),
                  'from' = c("person1@gmail.com", "person2@yahoo.com",
                             "person3@hotmail.com", "person4@msn.com"),
                  'to' = c("person2@yahoo.com,person3@hotmail.com", "person3@hotmail.com",
                           "person4@msn.com,person1@gmail.com,person2@yahoo.com", "person1@gmail.com"))



dat %>% separate(to, into = paste0("to_", 1:3), sep = ",", extra = "merge", fill = "right")
