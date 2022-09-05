data <- mtcars %>%
  tibble::rownames_to_column(var = "car_name") %>%
  tidyr::separate(car_name, sep = " ", into = c("model", "make"), extra = "merge")

library(htmltools)
library(reactable)

# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "0.875rem", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "0.375rem", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}


reactable(
  data %>%
    group_by(model) %>%
    summarise(avg_mpg = mean(mpg, na.rm = TRUE)) %>%
    ungroup(),
  defaultSorted = "avg_mpg",
  columns = list(
    model = colDef(
      name = "Model"
    ),
    avg_mpg = colDef(
      name = "Avg mpg",
      defaultSortOrder = "desc",
      format = colFormat(separators = TRUE),
      # Render the bar charts using a custom cell render function
      cell = function(value) {
        # Add thousands separators
        value <- format(value, big.mark = ",")
        value <- format(value, width = 4, justify = "left")
        bar_chart(value,  fill = "#3fc1c9")
      },
      # And left-align the columns
      align = "left"
    )
  )
)
