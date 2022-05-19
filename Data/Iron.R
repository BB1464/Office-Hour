############################################################################
############################################################################
###                                                                      ###
###                         ANALYSIS OF VARIANCE                         ###
###                                                                      ###
############################################################################
############################################################################

library(tidyverse)
library(readxl)
library(agricolae)
library(report)

data <- read_excel(here::here('Data/IronBioavailabilityF.xls'))

head(data)

data <- data %>% mutate(across(.cols = c(1:3),.fns = factor))

# Build the linear model
model <- lm(HC~Rep+Phases*Diet,data = data)

cv.model(model) # CV from the model

anova(model) # Anova output

report(anova(model)) # Brief Interpretation of the output

#summary(model)



# Post Hoc test

HSD.test(y = model,trt = 'Phases',console = TRUE)

HSD.test(y = model,trt = 'Diet',console = TRUE)


# Post Hoc for Interraction term

HSD.test(y = model,trt = c('Phases','Diet'),console = TRUE)


# Visualization of The Interraction Term

ggplot(data = data,aes(x = Diet,y = HC,fill=Phases))+
stat_summary(geom = 'errorbar',fun.data = mean_se,position = position_dodge(.9),size=.5)+
stat_summary(geom = 'col',position = 'dodge',fun = 'mean')+
  scale_fill_brewer(palette = 'Set1')+
  theme_test()+
  theme(axis.line = element_line(),legend.position = c(0.1,0.9),text = element_text(family = 'serif',size = 12))+
  labs(y='Haemoglobin Concentration')

# Export the image as png

ggsave('Haemoglobin.png',path = here::here('Plot'),width = 10,height = 6,dpi = 450)
