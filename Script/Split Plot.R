#Mixed model for analysis of split plot analysis

library(tidyverse)

# Import the dataset into R

T <- read_csv('Data/T.csv')



str(T)

T <- T %>% mutate_at(.vars = c(1:3),as.factor)

str(T)

library(lme4)
library(agricolae)

# splot-plot using lmer() function

attach(T)# Variety=main plot
SP <- lmer(GY ~ IRR + Variety + IRR:Variety
           + (1|Blocks ) + (1|Blocks :Variety))


SP2 <- lmer(GY ~ IRR + Variety
           + (1|Blocks ) + (1|Blocks :Variety))


SP3 <- lmer(GY ~ IRR+ (1|Blocks ) + (1|Blocks :Variety))


anova(SP)#opposed to p-values


#to get P value
library(car)
Anova(SP)
summary(SP)

Anova(SP2)


Anova(SP3)

# Pos- hoc analysis
library(multcomp)
library(dplyr)
library(emmeans)

# Variety
VS<- emmeans::emmeans(SP,~ Variety) %>%
  multcomp::cld(Letters=letters)
VS

# Irrigation
VS<- emmeans::emmeans(SP,~ IRR) %>%
  multcomp::cld(Letters=letters)
VS





IS <- emmeans::emmeans(SP3, ~ IRR) %>%
  multcomp::cld(Letters=letters)
IS

VIS <- emmeans::emmeans(SP, ~ Variety *IRR) %>%
  multcomp::cld(Letters=letters)
VIS


# graphical way
library(multcomp)

tuk3 <- glht(SP3, linfct = mcp(IRR = "Tukey"))


tuk.cld <- cld(tuk3)
old.par <- par(mai=c(1,1,1.25,1), no.readonly=TRUE)
plot(tuk.cld, col=2:6)
