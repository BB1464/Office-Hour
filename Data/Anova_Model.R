
############################################################################
############################################################################
###                                                                      ###
###                            DATA WRANGLING                            ###
###                                                                      ###
############################################################################
############################################################################


library(tidyverse)
library(readxl)


# Import the dataset into R

#dat1 <- read_excel(path = 'Data/Copy of Field data Mature trait.xlsx',sheet = 1)


#dat2 <- read_excel(path = 'Data/Copy of Mature trait data pot exp.xlsx',sheet = 2)


dat2 <- read_excel(path = here::here('Data/Copy of Field data Mature trait.xlsx'))



dat2 <- dat2 %>% filter(across(.cols = everything(),.fns = ~!is.na(.)))



# Data Wrangling

dat2 <- dat2 %>% mutate(across(.cols = c(1:2),.fns = factor)) %>%
  janitor::clean_names()

glimpse(dat2)

# Let me fit all the model at once

for (i in 3:ncol(dat2)) {
print(paste('----',names(dat2[i]),'----'))

model1 <- lm(dat2[[i]]~reps+unique_id,data=dat2)

print(anova(model1))

lala=agricolae::HSD.test(y = model1,trt = 'unique_id',console = TRUE)
print(lala$groups)


}
