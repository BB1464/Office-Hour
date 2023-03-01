
library(janitor)
library(tidyverse)
library(readxl)
library(flextable)
library(dlookr)



# Import the data set -----------------------------------------------------

dat <- read_excel(path = here::here('Data/Fungi Bambara Beulah.xlsx'))


# Category ----------------------------------------------------------------


dat %>%
     select(-Rep,-Accession,-Compartment) %>%
   diagnose() %>% flextable()



#
#
#
#
#
#
# head(dat)
#
# dat %>%
#   select(-Rep) %>%
#   pivot_longer(cols = c(3:13)) %>%
#   tabyl(name) %>%
#   adorn_totals("col") %>%
#   adorn_percentages() %>%
#   adorn_pct_formatting(digits = 2)
#
#
#
#
# # Second Approach ---------------------------------------------------------
#
#
# library(tidyverse) # for almost everything ;)
# library(flextable) # for beautifying tables
# library(dlookr)    # for the main event of the evening ;)
#
# dat %>%
#   select(-Rep,-Accession,-Compartment) %>%
# diagnose() %>% flextable()
#
#
# # Category ----------------------------------------------------------------
#
#
# dat %>%
#   select(-Rep,-Accession,-Compartment) %>%
#   pivot_longer(cols = c(1:11)) %>%
#   diagnose_category() %>% flextable()
#
#
#
