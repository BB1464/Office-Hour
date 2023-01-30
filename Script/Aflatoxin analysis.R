#Set Working Directory

setwd("C:/Users/UserGuy/Desktop/R Analysis new")

#Import your data

Faith<-read.csv("Aflatoxin analysis 8 12 2022.csv")

#Load library
library(moments)
library(ggpubr)
library(lmerTest)
library(tidyverse)
library(agricolae)
library(stats)
library(janitor)
library(car)
library(emmeans)
library(multcomp)

#Prepare dataset

Faith<-Faith %>% clean_names()
attach(Faith)

str(Faith)


# Check for the normal distribution
ggplot(data = Faith,aes(x = aflatoxin_level))+geom_density()+theme_classic()

shapiro.test(Faith$aflatoxin_level)

skewness(aflatoxin_level)


#Convert Integer and character to factor

## Clean up the Codes

dat1 <- Faith |>
  mutate(across(.cols = c(1:4),.fns = factor))


# dat1=Faith$location<-as.factor(Faith$location)
# dat1=Faith$sample_codes<-as.factor(Faith$sample_codes)
# dat1=Faith$replicates <- as.factor(Faith$replicates)
# dat1=Faith$aflatoxin_type <- as.factor(Faith$aflatoxin_type)
#
#Logarithm transformation of positively skewed data and logarithm data:

dat1=Faith %>% mutate(tran_aflatoxin_level = (log(aflatoxin_level)))


#view data
str(dat1)
names(dat1)
view(dat1)

##Combined effect on samples
# Linear model
## What about the Interraction terms?

lyd <-lm(tran_aflatoxin_level ~ replicates+
              sample_codes+
              aflatoxin_type+
              location, data = dat1)

#Anova test
anova(lyd)%>% knitr::kable()

#Post hoc
HSD.test(y = lyd,trt = 'sample_codes',console = TRUE)


##Let plot for Sample_codes (Box plot)
theme_set(theme_test())

ggplot(dat1, aes(x=sample_codes,y=tran_aflatoxin_level,fill=sample_codes))+
  geom_boxplot(outlier.shape = NA)+stat_boxplot(geom='errorbar')+
  stat_summary(geom = 'point',fun = 'mean',shape=19,position = position_dodge(.9),colour='red',size=3)+# Here I show the mean aflatoxin level on the boxplot with red colour
  geom_jitter()+ # You can remove this if you dont want the point on the plot
  labs(y="Aflatoxin level",x="Sample codes")+
  theme(axis.text.x = element_text(),
        axis.title = element_text(size=15,family = 'serif',face = 'bold',colour = 'black'),
        axis.text = element_text(size=12,family = 'serif',face = 'bold',colour = 'black'),legend.position ='none')+
  scale_fill_brewer(palette = 'Set2')


##Let plot for Sample_codes (Bar plot)
ggplot(dat1,aes(sample_codes,tran_aflatoxin_level,fill=sample_codes))+
  stat_summary(fun.data = 'mean_se',geom='errorbar',
               position = position_dodge(.9),width=.2,colour='black',size=0.6)+
  stat_summary(geom = 'col',position = 'dodge',fun = 'mean')+
  scale_y_continuous(expand = expansion(mult = c(0,0),add = c(0,1)))+theme_test()+
  theme(axis.text.x = element_text(family = 'serif',face = 'bold',colour = 'black',size = 15),legend.position = 'none',axis.text.y = element_text(family = 'serif',face = 'bold',colour = 'black',size = 15),axis.title = element_text(family = 'serif',face = 'bold',colour = 'black',size = 15))+
  scale_fill_manual(values = c('#CCCCCC','#666666','#000000'))+labs(y='Aflatoxin level',x='Sample codes')




#Save plot
ggsave('Aflatoxin.tiff',width = 12,height = 9,dpi = 300)
