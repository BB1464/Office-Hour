
library(raster)
library(ggplot2)

tmax_data <- getData(name = "worldclim", var = "tmax", res = 10)

tmax_data

gain(tmax_data) <- 0.1

tmax_data$tmax5


# Converting the raster object into a dataframe
tmax_data_may_df <- as.data.frame(tmax_data$tmax5, xy = TRUE, na.rm = TRUE)
rownames(tmax_data_may_df) <- c()

ggplot(
  data = tmax_data_may_df,
  aes(x = x, y = y)
) +
  geom_tile(aes(fill = tmax5)) + # you can also use geom_raster
  labs(
    title = "Maximum temperature in May",
    subtitle = "For the years 1970-2000"
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_gradientn(
    name = "Temperature (°C)",
    colours = c("#0094D1", "#68C1E6", "#FEED99", "#AF3301"),
    breaks = c(-20, 0, 20, 40)
  )+
  theme_void()
