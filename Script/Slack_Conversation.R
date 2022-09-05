
library(tidyverse)
library(paletteer)


dat_example <-
  data.frame(
    x1 = c(
      "PET",
      "Dog",
      "Cat",
      "Fish",
      "Rabbit",
      "Horse",
      "STATE",
      "CA",
      "NY",
      "NJ",
      "TX",
      "NM",
      "NJ",
      "LA",
      "WA",
      "SD",
      "MS",
      "LANGUAGE",
      "English",
      "Spanish"
    ),
    x2 = c(
      NA_character_,
      "10%",
      "20%",
      "20%",
      "20%",
      "30%",
      NA_character_,
      "5%",
      "5%",
      "10%",
      "10%",
      "10%",
      "10%",
      "10%",
      "10%",
      "10%",
      "20%",
      NA_character_,
      "40%",
      "60%"
    )
  )
cleaned_example <-
  dat_example %>%
  mutate(category = if_else(is.na(x2), x1, NA_character_), .before = x1) %>%
  tidyr::fill(category) %>%
  filter(!is.na(x2)) %>%
  rename(item = x1, pc = x2) %>%
  mutate(pc_num = parse_number(pc))

cleaned_example %>%
ggplot(aes(
           x = category,
           y = pc_num,
           fill = item,
           label = item
         )) +
           geom_col(show.legend = FALSE,
                    color = "black",
                    width = 1) +
           coord_flip() +
           xlab("") +
           geom_text(size = 3, position = position_stack(vjust = 0.5)) +
           theme(panel.background = element_blank(),
                 axis.ticks.y = element_blank()) +
           paletteer::scale_fill_paletteer_d("colorblindr::OkabeIto")+
           geom_col(show.legend = FALSE) +
  scale_fill_manual(values = col)
           paletteer::scale_fill_paletteer_d("colorblindr::OkabeIto")



head(cleaned_example)

