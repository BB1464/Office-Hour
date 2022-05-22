library(tidyverse)

data(mtcars)
head(mtcars)

sleep_mean_values %>%
  ggplot(aes(vore,mean_sleep))+
  geom_col()+
  geom_errorbar(aes(ymin = mean_sleep-sd_sleep,ymax=mean_sleep+sd_sleep),width=.2)




sleep_mean_values %>%
  ggplot(aes(vore,mean_sleep))+
  geom_errorbar(aes(ymin = mean_sleep-sd_sleep,ymax=mean_sleep+sd_sleep),width=.2)+geom_col()


# This approach uses geom_errorbar
sleep_mean_values %>%
  ggplot(aes(vore,mean_sleep,fill=vore))+
  geom_col(width = .9)+
  geom_errorbar(mapping = aes(ymin=mean_sleep-sd_sleep,ymax=mean_sleep+sd_sleep),width=.2,col='black',size=1)



# Second Approach with stat_summary
iris %>%
  ggplot(aes(Species,Sepal.Width))+
  stat_summary(fun  = 'mean',geom = 'col')+
  stat_summary(geom = 'errorbar',position = position_dodge(.9),fun.data = mean_se,width=0.5,size=1,col='red')
