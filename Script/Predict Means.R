
###########################################################################
###########################################################################
###                                                                     ###
###                      LOAD THE REQUIRED LIBRARY                      ###
###                                                                     ###
###########################################################################
###########################################################################

library(tidyverse)
library(readxl)
library(here)
library(predictmeans)

#dat <- read_excel('Data/NaCl experiment data.xlsx')

dat <- read_excel(file.choose())

glimpse(dat)

# Convert all independent variable to factors

dat <- dat %>%
  mutate_at(.vars = c(1:3),.funs = as.factor)

glimpse(dat)


# Convert Young Leaf ETR to numeric
dat <- dat %>%
  mutate_if(is.character,as.numeric)

# Check the dataset again
glimpse(dat)


###########################################################################
###########################################################################
###                                                                     ###
###              SELECT THE COLUMNS I WILL BE WORKING WITH              ###
###                                                                     ###
###########################################################################
###########################################################################

dat <- dat %>% select(1:12,-PGR,-LER)

dat$NaCl <- dat$NaCl <- factor(dat$NaCl,levels = c('0 mM','50 mM','160 mM'))

# Rename four column
dat <- dat %>% rename(Chl_a=`Chl a`,Chl_b=`Chl b`,ChI_a_b=`ChI a/b`)

# Let me fit the model for Salt Tollerance

mod1 <- lme(ST~Cultivar*NaCl,random = ~1|Rep/Cultivar,data=dat)

anova(mod1)

predictmeans(model = mod1,modelterm = 'Cultivar:NaCl',adj = 'tukey',plottitle = '',plotxlab = 'NaCl',plotylab = 'Salt Tolerance',barplot = TRUE)


# Stress Tolerance Index
mod2 <- lme(STI~Cultivar*NaCl,random = ~1|Rep/Cultivar,data=dat)

anova(mod2)

predictmeans(model = mod2,modelterm = 'Cultivar:NaCl',adj = 'tukey',plottitle = '',plotxlab = 'NaCl',plotylab = 'Salt Tolerance',barplot = TRUE)


# Stress Weighted Performance
mod3 <- lme(SWP~Cultivar*NaCl,random = ~1|Rep/Cultivar,data=dat)

anova(mod3)

predictmeans(model = mod3,modelterm = 'Cultivar:NaCl',adj = 'tukey',plottitle = '',plotxlab = 'NaCl',plotylab = 'Salt Tolerance',barplot = TRUE)


# `Chl a`
mod4 <- lme(Chl_a~Cultivar*NaCl,random = ~1|Rep/Cultivar,data=dat)

anova(mod4)

predictmeans(model = mod4,modelterm = 'Cultivar:NaCl',adj = 'tukey',plottitle = '',plotxlab = 'NaCl',plotylab = 'Salt Tolerance',barplot = TRUE)

# Chl_b
mod5 <- lme(Chl_b~Cultivar*NaCl,random = ~1|Rep/Cultivar,data=dat)

anova(mod5)

predictmeans(model = mod5,modelterm = 'Cultivar:NaCl',adj = 'tukey',plottitle = '',plotxlab = 'NaCl',plotylab = 'Salt Tolerance',barplot = TRUE)

# "ChI_a_b"
mod6 <- lme(ChI_a_b~Cultivar*NaCl,random = ~1|Rep/Cultivar,data=dat)

anova(mod6)

predictmeans(model = mod6,modelterm = 'Cultivar:NaCl',adj = 'tukey',plottitle = '',plotxlab = 'NaCl',plotylab = 'Salt Tolerance',barplot = TRUE)


# Caroteniod
mod7 <- lme(ChI_a_b~Cultivar*NaCl,random = ~1|Rep/Cultivar,data=dat)

anova(mod7)

predictmeans(model = mod7,modelterm = 'Cultivar:NaCl',adj = 'tukey',plottitle = '',plotxlab = 'NaCl',plotylab = 'Salt Tolerance',barplot = TRUE)
