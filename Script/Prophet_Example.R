###########################################################################
###########################################################################
###                                                                     ###
###                           PROPHET EXAMPLE                           ###
###                                                                     ###
###########################################################################
###########################################################################



# Load libraries
library(prophet)
library(RCurl)
library(ggplot2)

# Get the data
url <-'https://raw.github.com/facebook/prophet/master/examples/example_retail_sales.csv'
df <- read.csv(url)

# Explore the data
ggplot(data = df, aes(x = as.Date(ds), y = y)) +
  geom_line() +
  ggtitle('Retail sales') +
  scale_y_continuous(labels = scales::comma) +
  xlab('Time') +
  ylab('Sales') +
  theme_bw()


# Call prophet to generate the model and fit the model
# model <- prophet(df,
#                  growth = 'linear',
#                  n.changepoints = 25,
#                  seasonality.prior.scale = 10,
#                  changepoint.prior.scale = 0.05,
#                  fit = TRUE)

# secret tip: get holidays for a specific country
holidays <- prophet:::make_holidays_df(1992:2016, toupper('GB'))

# Call prophet to generate the model and fit the model with holidays
model2 <- prophet(df,
                  growth = 'linear',
                  n.changepoints = 25,
                  seasonality.prior.scale = 10,
                  changepoint.prior.scale = 0.05,
                  holidays = holidays,
                  fit = TRUE)

# Set prediction horizon
future <- make_future_dataframe(model2,
                                periods = 365)

tail(future, 12)

# Forecast
forecast <- predict(model2, future)

# Plot
plot(model2, forecast)

# Plot components
prophet_plot_components(model2, forecast)
