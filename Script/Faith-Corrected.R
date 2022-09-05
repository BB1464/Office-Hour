#Set Working Directory

#setwd("C:/Users/UserGuy/Desktop/R Analysis new")

#Import your data

Faith<-read.csv(here::here('Data/Detached Leaf Assay.csv'))



#Load library
library(moments)
library(ggpubr)
library(lmerTest)
library(tidyverse)
library(agricolae)
library(stats)
library(janitor)
library(car)

#Prepare dataset

Faith<-Faith %>% clean_names()
attach(Faith)
str(Faith)

#Convert Integer and character to factor

Faith <- Faith |> mutate(across(.cols = c(1:5),.fns = factor)) |>
  mutate(across(.cols = c(6:9),.fns = as.numeric))


glimpse(Faith)


# I commented out all the duplicated code

# Faith$cowpea_genotype<-as.factor(Faith$cowpea_genotype)
# Faith$age_of_leaves_in_weeks<-as.factor(Faith$age_of_leaves_in_weeks)
# Faith$trials<-as.factor(Faith$trials)
# Faith$isolate_code<-as.factor(Faith$isolate_code)
# Faith$replicates <- as.factor(Faith$replicates)
# Faith$ day_5_perc_disease_severity <- as.numeric(Faith$ day_5_perc_disease_severity)
# Faith$ day_7_perc_disease_severity <- as.numeric(Faith$ day_7_perc_disease_severity)
# Faith$ day_11_perc_disease_severity <- as.numeric(Faith$ day_11_perc_disease_severity)
# Faith$ day_14_perc_disease_severity <- as.numeric(Faith$ day_14_perc_disease_severity)

# Check for the normal distribution for day 5
ggplot(data = Faith,aes(x = Day.5_Perc_.disease.severity))+geom_density()+theme_classic()#Day 5

shapiro.test(Faith$Day.5_Perc_.disease.severity)

#view(Faith)

#Compute skewness
skewness(Faith$Day.5_Perc_.disease.severity)#Day 5

#arscin transformation of positively skewed data and percentage data:
dat2=Faith %>% mutate(tran_day_5_perc_disease_severity = (asin(sqrt(Day.5_Perc_.disease.severity/100))))#Day 7

#view data
str(dat2)
names(dat2)
view(dat2)

#Filter Genotype for day 5

IT10K10702<-dat2 %>% filter(Cowpea.Genotype=='IT10K-1070-2')

IT10K8173<-dat2 %>% filter(Cowpea.Genotype=='IT10K-817-3')

IT10K16823<-dat2 %>% filter(Cowpea.Genotype=='IT10K-1682-3')

CB27<-dat2 %>% filter(Cowpea.Genotype=='CB-27')

#Gaussian distriubtion for inoculation codes day 5

model <- glm(formula = tran_day_5_perc_disease_severity~  Replicates +
               Isolate.Code+
               Age.of.leaves.in.weeks+
               Trials + Cowpea.Genotype,family = 'gaussian',data = dat2)

summary(model)


#day_5_perc_rot for IT10K-1070-2
model1 <- glm(formula = tran_day_5_perc_disease_severity~ Replicates +
                Isolate.Code+
                Age.of.leaves.in.weeks+
                Trials,family = 'gaussian',data = IT10K10702)

summary(model1)

#day_5_perc_rot for IT10K8173
model2 <- glm(formula = tran_day_5_perc_disease_severity~ Replicates +
                Isolate.Code+
                Age.of.leaves.in.weeks+
                Trials,family = 'gaussian',data = IT10K8173)

summary(model2)

#day_5_perc_rot for IT10K16823
model3 <- glm(formula = tran_day_5_perc_disease_severity~ Replicates +
                Isolate.Code+
                Age.of.leaves.in.weeks+
                Trials,family = 'gaussian',data = IT10K16823)

summary(model3)

#day_5_perc_rot for CB27
model4 <- glm(formula = tran_day_5_perc_disease_severity~ Replicates +
                Isolate.Code+
                Age.of.leaves.in.weeks+
                Trials,family = 'gaussian',data = CB27)

summary(model4)

#Interactions for day 5

model5 <- glm(formula = tran_day_5_perc_disease_severity~ Replicates +
                Isolate.Code+
                Age.of.leaves.in.weeks+
                Trials + Cowpea.Genotype + Isolate.Code*Cowpea.Genotype,family = 'gaussian',data = dat2)

summary(model5)


# Check for the normal distribution for day 7
ggplot(data = Faith,aes(x = Day.7_Perc_.disease.severity))+geom_density()+theme_classic()#Day 5

shapiro.test(Faith$Day.7_Perc_.disease.severity)

#view(Faith)

#Compute skewness
skewness(Faith$Day.7_Perc_.disease.severity)#Day 7

#arscin transformation of positively skewed data and percentage data:
dat3=Faith %>% mutate(tran_day_7_perc_disease_severity = (asin(sqrt(Day.7_Perc_.disease.severity/100))))#Day 7

#view data
str(dat3)
names(dat3)
#view(dat3)

#Filter Genotype for day 7

IT10K10702<-dat3 %>% filter(Cowpea.Genotype=='IT10K-1070-2')

IT10K8173<-dat3 %>% filter(Cowpea.Genotype=='IT10K-817-3')

IT10K16823<-dat3 %>% filter(Cowpea.Genotype=='IT10K-1682-3')

CB27<-dat3 %>% filter(Cowpea.Genotype=='CB-27')

#Gaussian distriubtion for inoculation codes day 7
mode <- glm(formula = tran_day_7_perc_disease_severity~ Replicates +
              Isolate.Code+
              Age.of.leaves.in.weeks+
              Trials + Cowpea.Genotype,family = 'gaussian',data = dat3)

summary(mode)

#day_7_perc_rot for IT10K-1070-2
model6 <- glm(formula = tran_day_7_perc_disease_severity~ Replicates +
                Isolate.Code+
                Age.of.leaves.in.weeks+
                Trials,family = 'gaussian',data = IT10K10702)

summary(model6)

#day_7_perc_rot for IT10K8173
model7 <- glm(formula = tran_day_7_perc_disease_severity~ Replicates +
                Isolate.Code+
                Age.of.leaves.in.weeks+
                Trials,family = 'gaussian',data = IT10K8173)

summary(model7)

#day_7_perc_rot for IT10K16823
model8 <- glm(formula = tran_day_7_perc_disease_severity~ Replicates +
                Isolate.Code+
                Age.of.leaves.in.weeks+
                Trials,family = 'gaussian',data = IT10K16823)

summary(model8)


#day_7_perc_rot for CB27
model9 <- glm(formula = tran_day_7_perc_disease_severity~ Replicates +
                Isolate.Code+
                Age.of.leaves.in.weeks+
                Trials,family = 'gaussian',data = CB27)

summary(model9)



#Interactions for day 7
model10 <- glm(formula = tran_day_7_perc_disease_severity~ Replicates +
                 Isolate.Code+
                 Age.of.leaves.in.weeks+
                 Trials + Cowpea.Genotype + Isolate.Code*Cowpea.Genotype,family = 'gaussian',data = dat3)

summary(model10)



#Compute skewness
#skewness(day_11_perc_disease_severity)#Day 11

#arscin transformation of positively skewed data and percentage data:
dat4=Faith %>% mutate(tran_day_11_perc_disease_severity = (asin(sqrt(Day.11_Perc_.disease.severity/100))))#Day 11

#view data
str(dat4)
names(dat4)
#view(dat4)



#Filter Genotype for day 11

IT10K10702<-dat4 %>% filter(Cowpea.Genotype=='IT10K-1070-2')

IT10K8173<-dat4 %>% filter(Cowpea.Genotype=='IT10K-817-3')

IT10K16823<-dat4 %>% filter(Cowpea.Genotype=='IT10K-1682-3')

CB27<-dat4 %>% filter(Cowpea.Genotype=='CB-27')



#Gaussian distriubtion for inoculation codes day 11
modela <- glm(formula = tran_day_11_perc_disease_severity~ Replicates +
                Isolate.Code+
                Age.of.leaves.in.weeks+
                Trials + Cowpea.Genotype,family = 'gaussian',data = dat4)

summary(modela)

#day_11_perc_rot for IT10K-1070-2
model11 <- glm(formula = tran_day_11_perc_disease_severity~ Replicates +
                 Isolate.Code+
                 Age.of.leaves.in.weeks+
                 Trials,family = 'gaussian',data = IT10K10702)

summary(model11)

#day_11_perc_rot for IT10K8173
model12 <- glm(formula = tran_day_11_perc_disease_severity~ Replicates +
                 Isolate.Code+
                 Age.of.leaves.in.weeks+
                 Trials,family = 'gaussian',data = IT10K8173)

summary(model12)

#day_11_perc_rot for IT10K16823
model13 <- glm(formula = tran_day_11_perc_disease_severity~ Replicates +
                 Isolate.Code+
                 Age.of.leaves.in.weeks+
                 Trials,family = 'gaussian',data = IT10K16823)

summary(model13)

#day_11_perc_rot for CB27
model14 <- glm(formula = tran_day_11_perc_disease_severity~ Replicates +
                 Isolate.Code+
                 Age.of.leaves.in.weeks+
                 Trials,family = 'gaussian',data = CB27)

summary(model14)


#Interactions for day 11
model15 <- glm(formula = tran_day_11_perc_disease_severity~ Replicates +
                 Isolate.Code+
                 Age.of.leaves.in.weeks+
                 Trials + Cowpea.Genotype + Isolate.Code*Cowpea.Genotype,family = 'gaussian',data = dat4)

summary(model15)


#Compute skewness
skewness(Faith$Day.14_Perc_.disease.severity)#Day 14

#arscin transformation of positively skewed data and percentage data:
dat5=Faith %>% mutate(tran_day_14_perc_disease_severity = (asin(sqrt(Day.14_Perc_.disease.severity/100))))#Day 14

#view data
str(dat5)
names(dat5)
#view(dat5)


#Filter Genotype for day 14

IT10K10702<-dat5 %>% filter(Cowpea.Genotype=='IT10K-1070-2')

IT10K8173<-dat5 %>% filter(Cowpea.Genotype=='IT10K-817-3')

IT10K16823<-dat5 %>% filter(Cowpea.Genotype=='IT10K-1682-3')

CB27<-dat5 %>% filter(Cowpea.Genotype=='CB-27')

#Gaussian distriubtion for inoculation codes day 14
modea <- glm(formula = tran_day_14_perc_disease_severity~ Replicates +
               Isolate.Code+
               Age.of.leaves.in.weeks+
               Trials + Cowpea.Genotype,family = 'gaussian',data = dat5)

summary(modea)


#day_14_perc_rot for IT10K-1070-2
model16 <- glm(formula = tran_day_14_perc_disease_severity~ Replicates +
                 Isolate.Code+
                 Age.of.leaves.in.weeks+
                 Trials,family = 'gaussian',data = IT10K10702)

summary(model16)

#day_14_perc_rot for IT10K8173
model17 <- glm(formula = tran_day_14_perc_disease_severity~ Replicates +
                 Isolate.Code+
                 Age.of.leaves.in.weeks+
                 Trials,family = 'gaussian',data = IT10K8173)

summary(model17)


#day_14_perc_rot for IT10K16823
model18 <- glm(formula = tran_day_14_perc_disease_severity~ Replicates +
                 Isolate.Code+
                 Age.of.leaves.in.weeks+
                 Trials,family = 'gaussian',data = IT10K16823)

summary(model18)

#day_14_perc_rot for CB27
model19 <- glm(formula = tran_day_14_perc_disease_severity~ Replicates +
                 Isolate.Code+
                 Age.of.leaves.in.weeks+
                 Trials,family = 'gaussian',data = CB27)

summary(model19)


#Interactions for day 14
model20 <- glm(formula = tran_day_14_perc_disease_severity~ Replicates +
                 Isolate.Code+
                 Age.of.leaves.in.weeks+
                 Trials + Cowpea.Genotype +Isolate.Code*Cowpea.Genotype,family = 'gaussian',data = dat5)

summary(model20)

#Anova test
anova(model)
anova(mode)
anova(modea)
anova(modela)
anova(model1)
anova(model2)
anova(model3)
anova(model4)
anova(model5)
anova(model6)
anova(model7)
anova(model8)
anova(model9)
anova(model10)
anova(model11)
anova(model12)
anova(model13)
anova(model14)
anova(model15)
anova(model16)
anova(model17)
anova(model18)
anova(model19)
anova(model20)

#To add p value
Anova(model)%>% knitr::kable()
Anova(mode)%>% knitr::kable()
Anova(modea)%>% knitr::kable()
Anova(modela)%>% knitr::kable()
Anova(model1)%>% knitr::kable()
Anova(model2)%>% knitr::kable()
Anova(model3)%>% knitr::kable()
Anova(model4)%>% knitr::kable()
Anova(model5)%>% knitr::kable()
Anova(model6)%>% knitr::kable()
Anova(model7)%>% knitr::kable()
Anova(model8)%>% knitr::kable()
Anova(model9)%>% knitr::kable()
Anova(model10)%>% knitr::kable()
Anova(model11)%>% knitr::kable()
Anova(model12)%>% knitr::kable()
Anova(model13)%>% knitr::kable()
Anova(model14)%>% knitr::kable()
Anova(model15)%>% knitr::kable()
Anova(model16)%>% knitr::kable()
Anova(model17)%>% knitr::kable()
Anova(model18)%>% knitr::kable()
Anova(model19)%>% knitr::kable()
Anova(model20)%>% knitr::kable()
#Post hoc
HSD.test(y = mode,trt = 'isolate_code',console = TRUE)

HSD.test(y = modea,trt = 'isolate_code',console = TRUE)

HSD.test(y = modela,trt = 'isolate_code',console = TRUE)

HSD.test(y = model1,trt = 'isolate_code',console = TRUE)

HSD.test(y = model2,trt = 'isolate_code',console = TRUE)

HSD.test(y = model3,trt = 'isolate_code',console = TRUE)

HSD.test(y = model4,trt = 'isolate_code',console = TRUE)

HSD.test(y = model5,trt = 'isolate_code',console = TRUE)

HSD.test(y = model6,trt = 'isolate_code',console = TRUE)

HSD.test(y = model7,trt = 'isolate_code',console = TRUE)

HSD.test(y = model8,trt = 'isolate_code',console = TRUE)

HSD.test(y = model9,trt = 'isolate_code',console = TRUE)

HSD.test(y = model10,trt = 'isolate_code',console = TRUE)

HSD.test(y = model11,trt = 'isolate_code',console = TRUE)

HSD.test(y = model12,trt = 'isolate_code',console = TRUE)

HSD.test(y = model13,trt = 'isolate_code',console = TRUE)

HSD.test(y = model14,trt = 'isolate_code',console = TRUE)

HSD.test(y = model15,trt = 'isolate_code',console = TRUE)

HSD.test(y = model16,trt = 'isolate_code',console = TRUE)

HSD.test(y = model17,trt = 'isolate_code',console = TRUE)

HSD.test(y = model18,trt = 'isolate_code',console = TRUE)

HSD.test(y = model19,trt = 'isolate_code',console = TRUE)

HSD.test(y = model20,trt = 'isolate_code',console = TRUE)

