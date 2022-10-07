### Heat map
###


dat <- data.frame(temp = sample(20,100, replace=TRUE),
                  date=seq(as.Date("2011-07-01"), by=1, len=100))


require(ggplot2)

ggplot(dat, aes(x = date, y = 1)) +
  geom_tile(aes(fill = temp)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(y = NULL) +
  scale_y_continuous(breaks = NULL)
