library(tidyverse)
library(readxl)


dat <- read_excel(path = '~/../Desktop/dat.xlsx',skip = 1)

# Temp and Relative Humidity
tem <- dat |> filter(...1 %in%c('Ambient Temperature','Relative humidity'))


# Colour of the Animal
dat1 <- dat |> filter(...1!='Ambient Temperature') |>
  filter(...1!='Relative humidity') |> pivot_longer(-1)

# Join the two

# Split the variable column
dat3 <- dat1 |> separate(col = ...1,into = c('Colour','Rep'))


mod <- lm(value~Rep+Colour+name,data=dat3)
anova(mod)

HSD.test(y = mod,trt = 'name',console = TRUE)
