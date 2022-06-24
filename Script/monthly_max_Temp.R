
# Website
# https://rspatialdata.github.io/vegetation.html

library(raster)
library(ggplot2)
library(tidyverse)

# Downloading monthly maximum temperature data
tmax_data <- getData(name = "worldclim", var = "tmax", res = 10)

# Converting temperature values to Celcius
gain(tmax_data) <- 0.1

# Converting the raster object into a dataframe
tmax_data_df <- as.data.frame(tmax_data, xy = TRUE, na.rm = TRUE)
rownames(tmax_data_df) <- c()

# Renaming the month columns, Converting the dataframe into long format and converting month column into a factor
tmax_data_df_long <- tmax_data_df %>%
  rename(
    "January" = "tmax1", "February" = "tmax2", "March" = "tmax3", "April" = "tmax4",
    "May" = "tmax5", "June" = "tmax6", "July" = "tmax7", "August" = "tmax8",
    "September" = "tmax9", "October" = "tmax10", "November" = "tmax11", "December" = "tmax12"
  ) %>%
  pivot_longer(c(-x, -y), names_to = "month", values_to = "temp")


tmax_data_df_long$month <- factor(tmax_data_df_long$month, levels = month.name)

tmax_data_df_long %>%
  ggplot(aes(x = x, y = y)) +
  geom_raster(aes(fill = temp)) +
  facet_wrap(~month) +
  labs(
    title = "Monthly maximum temperatures",
    subtitle = "For the years 1970-2000"
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_gradientn(
    name = "Temperature (°C)",
    colours = c("#006D9B", "#0094D1", "#68C1E6", "#FEED99", "#AF3301"),
    breaks = c(-40, -20, 0, 20, 40)
  )

