library(ggplot2)

plots <- mtcars %>%
  split(.$cyl) %>%
  map(~ggplot(., aes(mpg, wt)) + geom_point())

paths <- stringr::str_c(names(plots), ".pdf")

pwalk(list(paths, plots), ggsave, path = tempdir())




library(tidyverse)

df <- tibble::tribble(
  ~transaction_id,   ~fee_name, ~fee_amount, ~fee_name_1, ~fee_amount_1, ~fee_name_3, ~fee_amount_3,
  1651651651L, "brokerage",         55L,       "tax",           69L,   "custody",           12L
)


df |> pivot_longer(cols = c(1,3,5,7))





library(tidyverse)

df <- tibble::tribble(
  ~transaction_id,   ~fee_name, ~fee_amount, ~fee_name_1, ~fee_amount_1, ~fee_name_3, ~fee_amount_3,
  1651651651L, "brokerage",         55L,       "tax",           69L,   "custody",           12L
)

col_groups <- list(c("fee_name", "fee_amount"),
                   c("fee_name_1", "fee_amount_1"),
                   c("fee_name_3", "fee_amount_3"))

map_df(col_groups,
       ~ df %>%
         select(transaction_id, all_of(.x)) %>%
         rename_with(function(x) str_remove(x, "_\\d$")))


# Data.table approach
library(data.table)


df <- tibble::tribble(
  ~transaction_id,   ~fee_name, ~fee_amount, ~fee_name_1, ~fee_amount_1, ~fee_name_3, ~fee_amount_3,
  1651651651L, "brokerage",         55L,       "tax",           69L,   "custody",           12L
)


setDT(df)

melt(df, measure.vars = patterns("name", "amount"), value.name = c("fee_name", "fee_amount"))
