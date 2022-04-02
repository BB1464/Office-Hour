

library(apsimx)
library(ggplot2)
ames.iem <- get_iem_apsim_met(lonlat = c(-93.77,42.02),dates = c("1990-01-01","2021-12-31"))


summary(ames.iem)

# Quick Visualization

plot(ames.iem,years = 2012:2015)


# Cummulative Plot

plot(ames.iem,years = 2012:2015,cumulative = TRUE,climatology = TRUE)


# Plot for Rainfall

plot(ames.iem,met.var = 'rain',years = 2012:2015,cumulative = TRUE,climatology = TRUE)
