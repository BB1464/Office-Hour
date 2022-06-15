library(gt)
library(tidyverse)

# Table reading
news_orgs <-
  readr::read_csv(
    file = 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv',
    col_types = cols(
      publication_name = col_character(),
      parent_publication = col_character(),
      url = col_character(),
      owner = col_character(),
      is_owner_founder = col_character(),
      city = col_character(),
      state = col_character(),
      country = col_character(),
      primary_language = col_character(),
      primary_language_other = col_logical(),
      tax_status_founded = col_character(),
      tax_status_current = col_character(),
      year_founded = col_double(),
      total_employees = col_character(),
      budget_percent_editorial = col_character(),
      budget_percent_revenue_generation = col_character(),
      budget_percent_product_technology = col_character(),
      budget_percent_administration = col_character(),
      products = col_character(),
      products_other = col_character(),
      distribution = col_character(),
      distribution_method_other = col_character(),
      geographic_area = col_character(),
      core_editorial_strategy_characteristics = col_character(),
      core_editorial_strategy_characteristics_other = col_character(),
      coverage_topics = col_character(),
      coverage_topics_other = col_logical(),
      underrepresented_communities = col_character(),
      underrepresented_communities_not_listed = col_character(),
      revenue_streams = col_character(),
      revenue_stream_other = col_logical(),
      revenue_stream_additional_info = col_logical(),
      revenue_stream_largest = col_character(),
      revenue_streams_largest_other = col_character(),
      paywall_or_gateway = col_character(),
      paywall_or_gateway_other = col_logical(),
      advertising_products = col_character(),
      advertising_product_other = col_logical(),
      real_world_impacts = col_character(),
      summary = col_character()
    ))

#
# Region Groupings (for grouping within the gt table)
#

west <-
  c(
    "WA", "OR", "CA", "NV", "ID", "MT", "WY",
    "CO", "UT", "AZ", "NM", "AK", "HI"
  )

midwest <-
  c(
    "ND", "SD", "NE", "KS", "MN", "IA", "MO",
    "WI", "IL", "MI", "IN", "OH"
  )

south <-
  c(
    "TX", "OK", "AR", "LA", "MS", "TN", "AL",
    "GA", "FL", "SC", "NC", "KY", "WV", "VA",
    "MD", "DC", "DE", "PR"
  )

northeast <- c("PA", "NY", "NJ", "VT", "MA", "CT", "RI", "NH", "ME")

canada <-
  c(
    "BC", "AB", "SK", "MB", "ON", "QC", "NB",
    "NS", "NT", "NL", "NU", "PE", "YT"
  )

# Table transformation
news_orgs_summary <-
  news_orgs %>%
  filter(!is.na(url)) %>%
  filter(!is.na(summary)) %>%
  filter(!is.na(state)) %>%
  select(
    publication_name, url, state, city,
    year_founded, tax_status_current, primary_language
  ) %>%
  mutate(region = case_when(
    state %in% west ~ "West",
    state %in% midwest ~ "Midwest",
    state %in% northeast ~ "Northeast",
    state %in% south ~ "South",
    state %in% canada ~ "Canada"
  )) %>%
  mutate(tax_status_current = case_when(
    tax_status_current == "For Profit" ~ "FP",
    tax_status_current == "Not for Profit" ~ "NFP",
    tax_status_current == "LLC" ~ "LLC",
    grepl("benefit", tax_status_current) ~ "PBC",
    TRUE ~ "Other"
  )) %>%
  mutate(primary_language = case_when(
    is.na(primary_language) ~ "EN",
    primary_language == "English" ~ "EN",
    primary_language == "Spanish" ~ "ES",
    grepl("English", primary_language) &
      grepl("Spanish", primary_language) ~ "EN/ES",
    TRUE ~ primary_language
  ))

# The gt table
news_orgs_summary %>%
  gt(groupname_col = "region") %>%
  cols_merge(
    columns = c(publication_name, url),
    pattern = "{1}<br><span style='font-size:smaller'>{2}</span>"
  ) %>%
  cols_label(
    publication_name = "Publication",
    primary_language = "Lang",
    tax_status_current = md("Type of<br>Org."),
    year_founded = md("Started<br>in")
  ) %>%
  tab_footnote(
    footnote = "Includes S corps, partnerships, 501c(3) entities,
    and sole proprietors.",
    locations = cells_body(
      columns = tax_status_current,
      rows = tax_status_current == "Other"
    )
  ) %>%
  cols_width(city ~ px(200)) %>%
  tab_style(
    style = cell_text(size = "smaller"),
    locations = cells_body(columns = city)
  ) %>%
  opt_all_caps() %>%
  opt_table_font(font = google_font(name = "Karla"))
