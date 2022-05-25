###########################################################################
###########################################################################
###                                                                     ###
###                               GT DEMO                               ###
###                                                                     ###
###########################################################################
###########################################################################


# libraries
library(gt) # version 0.5.0
library(dplyr)
library(palmerpenguins)

# Create a gt table
penguins %>%
  filter(sex == 'female') %>%
  select(-year) %>%
  group_by(island, species) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  gt() %>%
  tab_header(
    title = "Size measurements for adult foraging penguins 🐧",
    subtitle = "Palmer Station, Antarctica"
  ) %>%
  cols_label(
    species = "Species",
    bill_length_mm = "Bill length",
    bill_depth_mm = "Bill depth",
    flipper_length_mm = 'Flipper length',
    body_mass_g = 'Body mass'
  ) %>%
  fmt_number(columns = where(is.numeric),
             decimals = 1) %>%
  opt_row_striping(row_striping = TRUE) %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_row_groups()
  ) %>%
  tab_footnote(footnote = 'Averages for the years 2007, 2008 and 2009 combined',
               locations = cells_title(groups = c("title"))) %>%
  tab_footnote(footnote = 'Stated in grams',
               locations = cells_column_labels(
                 columns = c(body_mass_g)
               )) %>%
  tab_footnote(footnote = 'Stated in mm',
               locations = cells_column_labels(
                 columns = c(bill_length_mm,
                             bill_depth_mm,
                             flipper_length_mm)
               )) %>%
  cols_align(
    align = c("left"),
    columns = everything()
  )
