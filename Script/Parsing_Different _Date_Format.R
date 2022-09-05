
library(dplyr)
library(lubridate)

dat <- tribble(
  ~case_id, ~date_str, ~date_format,
  1, "2021-01-03", "ymd",
  2, "04-12-2021", "mdy",
  3, "2021-05-10", "ymd",
  4, "13-06-2022", "dmy"
)


dat |>
  mutate(
    date_convert = parse_date_time(date_str, date_format)
  )
