
library(tidyverse)
library(readxl)


excel_sheets('Data/Kehinde_Data.xlsx')

data <- read_excel('Data/Kehinde_Data.xlsx',sheet = 1)



Plot <- data |> pivot_longer(-c(1:3)) |> mutate(across(.cols = c(1:4),.fns = factor)) |> group_by(name) |> nest() |> mutate(plot=map2(.x = data,.y = name,.f = ~ggplot(data = .x,aes(x = week,y = value,fill=Genotype))+geom_col(position = position_dodge(.9))+facet_grid(~Nutrient)+
                                                                              labs(y=.y)))


map2(paste(.x = Plot$name,'.png'),.y = Plot$plot,.f = ggsave,width=10,height=7,dpi=450)


ggplot(data = data,aes(x = week,y = `No Leaves`,fill=Genotype))+
stat_summary(geom = 'errorbar',fun.data = mean_se,position = position_dodge(.9))+stat_summary(geom = 'col',fun = 'mean',position = 'dodge')+
facet_grid(~Nutrient)



ggplot(data = data,aes(x = week,y = `No Nodes`,fill=Nutrient))+
  stat_summary(geom = 'col',fun = 'mean',position = 'dodge')+
  stat_summary(geom = 'errorbar',fun.data = mean_se,position = position_dodge(.5),width=.5)
