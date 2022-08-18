library(tidyverse)
library(firatheme)

mpg_sum <- mpg %>%
  ## just use 2008 data
  dplyr::filter(year == 2008) %>%
  ## turn into lumped factors with capitalized names
  dplyr::mutate(
    manufacturer = stringr::str_to_title(manufacturer),
    manufacturer = forcats::fct_lump(manufacturer, n = 10)
  ) %>%
  ## add counts
  dplyr::count(manufacturer, sort = TRUE) %>%
  ## order factor levels by number, put "Other" to end
  dplyr::mutate(
    manufacturer = forcats::fct_rev(forcats::fct_inorder(manufacturer)),
    manufacturer = forcats::fct_relevel(manufacturer, "Other", after = 0)
  )

mpg_sum

levels(mpg_sum$manufacturer)

# Plotting

ggplot(mpg_sum, aes(x = n, y = manufacturer)) +
  ## draw bars
  geom_col(fill = "gray70") +
  ## change plot appearance
  theme_minimal()

# Calculate the percentage

mpg_sum <- mpg_sum %>%
  ## add percentage label with `sprintf()`
  dplyr::mutate(perc = paste0(sprintf("%4.1f", n / sum(n) * 100), "%"))

mpg_sum

# Second Approach using the scales package
mpg_sum %>%
  ## add percentage label with `scales::percent()`
  dplyr::mutate(perc = scales::percent(n / sum(n), accuracy = .1, trim = FALSE))


# Add the prepared label to the plot
ggplot(mpg_sum, aes(x = n, y = manufacturer)) +
  geom_col(fill = "gray70") +
  ## add percentage labels
  geom_text(aes(label = perc)) +
  theme_minimal()

# Add Description to One of the Bars
mpg_sum <- mpg_sum %>%
  dplyr::mutate(
    perc = paste0(sprintf("%4.1f", n / sum(n) * 100), "%"),
    ## customize label for the first category
    perc = if_else(row_number() == 1, paste(perc, "of all car models"), perc)
  )

ggplot(mpg_sum, aes(x = n, y = manufacturer)) +
  geom_col(fill = "gray70") +
  geom_text(aes(label = perc)) +
  ## make sure labels doesn't get cut
  scale_x_continuous(limits = c(NA, 24)) +
  theme_minimal()


## prepare non-aggregated data set with lumped and ordered factors
mpg_fct <- mpg %>%
  dplyr::filter(year == 2008) %>%
  dplyr::mutate(
    ## add count to calculate percentages later
    total = dplyr::n(),
    ## turn into lumped factors with capitalized names
    manufacturer = stringr::str_to_title(manufacturer),
    manufacturer = forcats::fct_lump(manufacturer, n = 10),
    ## order factor levels by number, put "Other" to end
    manufacturer = forcats::fct_rev(forcats::fct_infreq(manufacturer)),
    manufacturer = forcats::fct_relevel(manufacturer, "Other", after = 0)
  )

ggplot(mpg_fct, aes(x = manufacturer)) +
  geom_bar(fill = "gray70") +
  ## add count labels
  geom_text(
    stat = "count",
    aes(label = ..count..)
  ) +
  ## rotate plot
  coord_flip()  +
  theme_minimal()


# Adjust the Text Label
ggplot(mpg_sum, aes(x = n, y = manufacturer)) +
  geom_col(fill = "gray70") +
  geom_text(
    aes(label = perc),
    ## make labels left-aligned
    hjust = 1, nudge_x = -.5
  ) +
  theme_minimal()

## create color palette based on input data
pal <- c(
  "gray85",
  rep("gray70", length(mpg_sum$manufacturer) - 4),
  "coral2", "mediumpurple1", "goldenrod1"
)

# Second Approach for the pals
pal <- c('gray85',rep('gray70',7),'coral2','mediumpurple1','goldenrod1')

# Add the colours
ggplot(mpg_sum, aes(x = n, y = manufacturer,
                    fill = manufacturer)) +
  geom_col() +
  geom_text(
    aes(label = perc),
    hjust = 1, nudge_x = -.5
  ) +
  ## add custom colors
  scale_fill_manual(values = pal, guide = "none") +
  theme_minimal()

# We Can Add the Colour to the Dataset then we then map the fill
mpg_sum <-
  mpg_sum %>%
  mutate(
    color = case_when(
      row_number() == 1 ~ "goldenrod1",
      row_number() == 2 ~ "mediumpurple1",
      row_number() == 3 ~ "coral2",
      manufacturer == "Other" ~ "gray85",
      ## all others should be gray
      TRUE ~ "gray70"
    )
  )

ggplot(mpg_sum, aes(x = n, y = manufacturer, fill = color)) +
  geom_col() +
  geom_text(
    aes(label = perc),
    hjust = 1, nudge_x = -.5
  ) +
  ## add custom colors
  scale_fill_identity(guide = "none") +
  theme_minimal()


# Adjust the Text
ggplot(mpg_sum, aes(x = n, y = manufacturer, fill = color)) +
  geom_col() +
  geom_text(
    aes(label = perc),
    hjust = 1, nudge_x = -.5,
    size = 4, fontface = "bold") +
  ## reduce spacing between labels and bars
  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_identity(guide = "none") +
  ## get rid of all elements except y axis labels + adjust plot margin
  theme_void() +
  theme(axis.text.y = element_text(size = 14, hjust = 1),plot.margin = margin(rep(15, 4)))


# Adjust the Text Label

ggplot(mpg_sum, aes(x = n, y = manufacturer, fill = color)) +
  geom_col() +
  geom_label(
    aes(label = perc),
    hjust = 1, nudge_x = -.5,
    size = 4, fontface = "bold",
    ## turn into white box without outline
    fill = "white", label.size = 0
  ) +
  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_identity(guide = "none") +
  theme_void() +
  theme(
    axis.text.y = element_text(size = 14, hjust = 1),
    plot.margin = margin(rep(15, 4))
  )

# Re-Adjust the label

mpg_sum %>%
  mutate(
    ## set justification based on data
    ## so that only the first label is placed inside
    place = if_else(row_number() == 1, 1, 0),
    ## add some spacing to labels since we cant use nudge_x anymore
    perc = paste(" ", perc, " ")
  ) %>%
  ggplot(aes(x = n, y = manufacturer, fill = color)) +
  geom_col() +
  geom_text(
    aes(label = perc, hjust = place),
    size = 4, fontface = "bold", family = "Fira Sans"
  ) +
  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_identity(guide = "none") +
  theme_void() +
  theme(
    axis.text.y = element_text(size = 14, hjust = 1, family = "Fira Sans"),
    plot.margin = margin(rep(15, 4))
  )

# Different Approach
mpg_sum %>%
  ## overwrite old percentage labels
  mutate(
    perc=scales::percent(n/sum(n),accuracy = .1,trim = FALSE),
    #perc = paste0(sprintf("%4.1f", n / sum(n) * 100), "%"),
    perc = if_else(row_number() == 2, paste(perc, "of all car models"), perc)
  ) %>%
  ggplot(aes(x = n, y = manufacturer, fill = color)) +
  geom_col() +
  geom_text(
    aes(label = perc,),
    hjust = 0, nudge_x = .5,
    size = 4, fontface = "bold", family = "Fira Sans"
  ) +
  ## make sure labels doesn't get cut, part 1
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_identity(guide = "none") +
  theme_void() +
  theme(
    axis.text.y = element_text(size = 14, hjust = 1, family = "Fira Sans"),
    ## make sure labels doesn't get cut, part 2
    plot.margin = margin(15, 30, 15, 15)
  )
