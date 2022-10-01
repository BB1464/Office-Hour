library(tidyverse)
library(glue)
library(scales)


scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}


fi_plots <- iris %>%
  group_by(Species) %>%
  nest() %>%
  mutate(plot = pmap(.l = list(Species, data), .f = ~{
    ggplot(..2, aes(Sepal.Length, Sepal.Width)) +
      geom_point() +
      geom_line() +
      labs(title = glue("Slope of relation between adult education levels and {.x} by year"),
           x = "Year",
           y = "Slope") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90)) +
      scale_y_continuous(labels = scientific_10)
  })
  )

fi_plots


fi_plots$plot


