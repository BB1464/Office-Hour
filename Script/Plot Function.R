library(tidyverse)
library(rio)
library(readxl)
library(agricolae)
library(here)
library(broom)


# Morning High THI

dat <- import_list(file = here::here('Data/Morning High THI.xls'))

data1 <- dat[[1]]


# for (i in 5:ncol(data1)) {
#   print(paste('----',names(data1[i]),'----'))
#
# mod <- lm(data1[[i]]~Colour*Sex,data=data1)
# print(anova(mod))
#
# lala=HSD.test(y = mod,trt = 'Colour',console = TRUE,main = 'Colour')
# lala=HSD.test(y = mod,trt = 'Sex',console = TRUE,main = 'Sex')
# print(lala$comparison)
# }


# resp <- names(data1[c(5:ncol(data1))])
#
# exp <- names(data1[c(1:2)])
#
# resp <- set_names(resp)
#
# exp <- set_names(exp)

#
# plot_func <- function(x,y){
#   ggplot(data = data1,aes(x = .data[[x]],y = .data[[y]],fill=Sex))+
#   stat_summary(geom = 'errorbar',position = position_dodge(.9),fun.data = mean_se,width=0.7)+
# stat_summary(geom = 'col',position = 'dodge',fun = 'mean')+
# scale_fill_brewer(palette = 'Dark2')+
#     theme_light()+
#     labs(y='Respiratory Rate')
# }



# plot_func(x = 'Colour',y = 'RESPRT1')



exp_resp <- tidyr::expand_grid(exp,resp)

# Loop through the two vector with purrr

pmap(.l = exp_resp,.f = ~plot_func(x = .x,y = .y))






# Morning High THI

# Respiratory Rate

dat <- import_list(file = here::here('Data/Morning High THI.xls'))

data1 <- dat[[1]]



Filter <-(c("RESPRT1", "RESPRT8","RESPRT9","RESPRT10","RESPRT11","RESPRT12","RESPRT13","RESPRT14","RESPRT15","RESPRT16","RESPRT17","RESPRT18","RESPRT19","RESPRT20","RESPRT21","RESPRT22","RESPRT23","RESPRT24","RESPRT25","RESPRT26","RESPRT27","RESPRT28","RESPRT29","RESPRT32","RESPRT46","RESPRT48","RESPRT54","RESPRT57","RESPRT59","RESPRT62","RESPRT65","RESPRT66","RESPRT68","RESPRT74","RESPRT81","RESPRT82","RESPRT88"))



# Correct

# Respiratory Rate

Plot <- data1 |>
  pivot_longer(-c(1:4)) |>
  group_by(name) |>
  nest() |>
  filter(name %in% Filter) |>
  mutate(Plot = map2(.x = data, .y = name, .f = ~ ggplot(data = .x, aes(x = Colour, y = value, fill = Sex)) +
    stat_summary(geom = "errorbar", position = position_dodge(.9), fun.data = mean_se,width=0.7,col='black',size=0.7) +
    stat_summary(geom = "col", position = "dodge", fun = "mean") +
    theme_light() +
    scale_fill_brewer(palette = "Dark2") +
    labs(y = "Respiratory rate")))

print(Plot$Plot)




# Save the plot to path

map2(paste(.x=Plot$name,'.png'),.y=Plot$Plot,.f = ggsave,height=6,width=10,dpi=400,path=here::here('Plot/Morning High/Respiratory_Rate'))





# Pulse Rate

data1 <- dat[[2]]



Filter <-(c("PULRAT1", "PULRAT2","PULRAT3","PULRAT4","PULRAT5","PULRAT6","PULRAT7","PULRAT9","PULRAT10","PULRAT11","PULRAT12","PULRAT13","PULRAT14","PULRAT15","PULRAT16","PULRAT17","PULRAT18","PULRAT19","PULRAT20","PULRAT21","PULRAT22","PULRAT23","PULRAT24","PULRAT25","PULRAT26","PULRAT27","PULRAT28","PULRAT29","PULRAT30","PULRAT31","PULRAT32","PULRAT33","PULRAT34","PULRAT35","PULRAT36","PULRAT37","PULRAT38","PULRAT39","PULRAT40","PULRAT41","PULRAT42","PULRAT43","PULRAT44","PULRAT45","PULRAT46","PULRAT47","PULRAT48","PULRAT49","PULRAT50","PULRAT51","PULRAT52","PULRAT53","PULRAT54","PULRAT55","PULRAT56","PULRAT57","PULRAT58","PULRAT59","PULRAT60","PULRAT61","PULRAT62","PULRAT65","PULRAT66","PULRAT67","PULRAT70","PULRAT72","PULRAT73","PULRAT75","PULRAT77","PULRAT78","PULRAT79","PULRAT80","PULRAT81","PULRAT83","PULRAT84","PULRAT85","PULRAT86","PULRAT87","PULRAT88","PULRAT89"))



# Correct

Plot <- data1 |>
  pivot_longer(-c(1:4)) |>
  group_by(name) |>
  nest() |>
  filter(name %in% Filter) |>
  mutate(Plot = map2(.x = data, .y = name, .f = ~ ggplot(data = .x, aes(x = Colour, y = value, fill = Sex)) +
                       stat_summary(geom = "errorbar", position = position_dodge(.9), fun.data = mean_se,width=0.7,col='black',size=0.7) +
                       stat_summary(geom = "col", position = "dodge", fun = "mean") +
                       theme_light() +
                       scale_fill_brewer(palette = "Dark2") +
                       labs(y = "Pulse rate")))

print(Plot$Plot)




# Save the plot to path

map2(paste(.x=Plot$name,'.png'),.y=Plot$Plot,.f = ggsave,height=6,width=10,dpi=400,path='Plot/Morning High/Pulse_Rate')


# Rectal Temperature

data1 <- dat[[3]]



Filter <-(c("RECTEM1", "RECTEM8","RECTEM9","RECTEM10","RECTEM11","RECTEM12","RECTEM13","RECTEM14","RECTEM15","RECTEM16","RECTEM17","RECTEM18","RECTEM19","RECTEM20","RECTEM21","RECTEM22","RECTEM23","RECTEM24","RECTEM25","RECTEM26","RECTEM27","RECTEM28","RECTEM29","RECTEM32","RECTEM46","RECTEM48","RECTEM54","RECTEM57","RECTEM59","RECTEM62","RECTEM65","RECTEM66","RECTEM68","RECTEM70","RECTEM74","RECTEM81","RECTEM82","RECTEM88"))



# Correct

# Rectal Temperature


Plot <- data1 |>
  pivot_longer(-c(1:4)) |>
  group_by(name) |>
  nest() |>
  filter(name %in% Filter) |>
  mutate(Plot = map2(.x = data, .y = name, .f = ~ ggplot(data = .x, aes(x = Colour, y = value, fill = Sex)) +
                       stat_summary(geom = "errorbar", position = position_dodge(.9), fun.data = mean_se,width=0.7,col='black',size=0.7) +
                       stat_summary(geom = "col", position = "dodge", fun = "mean") +
                       theme_light() +
                       scale_fill_brewer(palette = "Dark2") +
                       labs(y = "Rectal temperature")))

print(Plot$Plot)




# Save the plot to path

map2(paste(.x=Plot$name,'.png'),.y=Plot$Plot,.f = ggsave,height=6,width=10,dpi=400,path='Plot/Morning High/Rectal_Temperature')




# Second Sheet


# Morning Low THI



dat <- import_list(here::here('Data/Morning Low THI.xls'))


data1 <- dat[[1]]


Filter <-(c("RESPRT8","RESPRT10","RESPRT11","RESPRT12","RESPRT14","RESPRT15","RESPRT16","RESPRT17","RESPRT19","RESPRT20","RESPRT21","RESPRT22","RESPRT23","RESPRT24","RESPRT25","RESPRT26","RESPRT27","RESPRT28","RESPRT54","RESPRT62","RESPRT65","RESPRT68","RESPRT74","RESPRT81","RESPRT82"))


# Respiratory Rate

Plot <- data1 |>
  pivot_longer(-c(1:4)) |>
  group_by(name) |>
  nest() |>
  filter(name %in% Filter) |>
  mutate(Plot = map2(.x = data, .y = name, .f = ~ ggplot(data = .x, aes(x = Colour, y = value, fill = Sex)) +
                       stat_summary(geom = "errorbar", position = position_dodge(.9), fun.data = mean_se,width=0.7,col='black',size=0.7) +
                       stat_summary(geom = "col", position = "dodge", fun = "mean") +
                       theme_light() +
                       scale_fill_brewer(palette = "Dark2") +
                       labs(y = "Respiratory rate")))

print(Plot$Plot)




# Save the plot to path

map2(paste(.x=Plot$name,'.png'),.y=Plot$Plot,.f = ggsave,height=6,width=10,dpi=400,path='Plot/Morning Low/Respiratory_Rate')





# Pulse Rate

data1 <- dat[[2]]



Filter <-(c("PULRAT1", "PULRAT2","PULRAT4","PULRAT5","PULRAT9","PULRAT10","PULRAT11","PULRAT12","PULRAT13","PULRAT14","PULRAT15","PULRAT16","PULRAT17","PULRAT18","PULRAT19","PULRAT20","PULRAT21","PULRAT22","PULRAT23","PULRAT24","PULRAT25","PULRAT27","PULRAT28","PULRAT29","PULRAT30","PULRAT31","PULRAT32","PULRAT33","PULRAT34","PULRAT35","PULRAT36","PULRAT37","PULRAT38","PULRAT39","PULRAT40","PULRAT41","PULRAT42","PULRAT43","PULRAT44","PULRAT45","PULRAT46","PULRAT47","PULRAT48","PULRAT49","PULRAT50","PULRAT51","PULRAT52","PULRAT53","PULRAT54","PULRAT55","PULRAT56","PULRAT57","PULRAT59","PULRAT60","PULRAT62","PULRAT66","PULRAT67","PULRAT70","PULRAT72","PULRAT73","PULRAT75","PULRAT77","PULRAT78","PULRAT79","PULRAT80","PULRAT81","PULRAT83","PULRAT84","PULRAT85","PULRAT86","PULRAT87","PULRAT88","PULRAT89"))



# Correct

Plot <- data1 |>
  pivot_longer(-c(1:4)) |>
  group_by(name) |>
  nest() |>
  filter(name %in% Filter) |>
  mutate(Plot = map2(.x = data, .y = name, .f = ~ ggplot(data = .x, aes(x = Colour, y = value, fill = Sex)) +
                       stat_summary(geom = "errorbar", position = position_dodge(.9), fun.data = mean_se,width=0.7,col='black',size=0.7) +
                       stat_summary(geom = "col", position = "dodge", fun = "mean") +
                       theme_light() +
                       scale_fill_brewer(palette = "Dark2") +
                       labs(y = "Pulse rate")))

print(Plot$Plot)


# Save the plot to path

map2(paste(.x=Plot$name,'.png'),.y=Plot$Plot,.f = ggsave,height=6,width=10,dpi=400,path='Plot/Morning Low/Pulse_Rate')




# Rectal Temperature

data1 <- dat[[3]]



Filter <-(c("RECTEM8", "RECTEM10","RECTEM11","RECTEM12","RECTEM14","RECTEM15","RECTEM16","RECTEM17","RECTEM19","RECTEM20","RECTEM21","RECTEM22","RECTEM23","RECTEM24","RECTEM25","RECTEM26","RECTEM27","RECTEM28","RECTEM54","RECTEM62","RECTEM65","RECTEM68","RECTEM74","RECTEM81","RECTEM82"))



# Correct

# Rectal Temperature


Plot <- data1 |>
  pivot_longer(-c(1:4)) |>
  group_by(name) |>
  nest() |>
  filter(name %in% Filter) |>
  mutate(Plot = map2(.x = data, .y = name, .f = ~ ggplot(data = .x, aes(x = Colour, y = value, fill = Sex)) +
                       stat_summary(geom = "errorbar", position = position_dodge(.9), fun.data = mean_se,width=0.7,col='black',size=0.7) +
                       stat_summary(geom = "col", position = "dodge", fun = "mean") +
                       theme_light() +
                       scale_fill_brewer(palette = "Dark2") +
                       labs(y = "Rectal temperature")))

print(Plot$Plot)




# Save the plot to path

map2(paste(.x=Plot$name,'.png'),.y=Plot$Plot,.f = ggsave,height=6,width=10,dpi=400,path='Plot/Morning Low/Rectal_Temperature')




# Afternoon High

dat <- import_list(here::here('Data/Afternoon High THI.xls'))

data1 <- dat[[1]]



Filter <-(c("RESPRT1", "RESPRT2","RESPRT3","RESPRT4","RESPRT5","RESPRT6","RESPRT7","RESPRT8","RESPRT9","RESPRT10","RESPRT11","RESPRT12","RESPRT13","RESPRT14","RESPRT15","RESPRT16","RESPRT18","RESPRT20","RESPRT21","RESPRT24","RESPRT27","RESPRT28","RESPRT29","RESPRT30","RESPRT31","RESPRT34","RESPRT36","RESPRT37","RESPRT38","RESPRT39","RESPRT40","RESPRT41","RESPRT42","RESPRT45","RESPRT46","RESPRT47","RESPRT48","RESPRT50","RESPRT51","RESPRT52","RESPRT54","RESPRT55","RESPRT56","RESPRT57","RESPRT58","RESPRT61","RESPRT62","RESPRT63","RESPRT66","RESPRT68","RESPRT69","RESPRT70","RESPRT71","RESPRT73","RESPRT74","RESPRT75","RESPRT77","RESPRT78","RESPRT79","RESPRT80","RESPRT81","RESPRT83","RESPRT85","RESPRT86","RESPRT89","RESPRT90"))



# Correct

# Respiratory Rate

Plot <- data1 |>
  pivot_longer(-c(1:4)) |>
  group_by(name) |>
  nest() |>
  filter(name %in% Filter) |>
  mutate(Plot = map2(.x = data, .y = name, .f = ~ ggplot(data = .x, aes(x = Colour, y = value, fill = Sex)) +
                       stat_summary(geom = "errorbar", position = position_dodge(.9), fun.data = mean_se,width=0.7,col='black',size=0.7) +
                       stat_summary(geom = "col", position = "dodge", fun = "mean") +
                       theme_light() +
                       scale_fill_brewer(palette = "Dark2") +
                       labs(y = "Respiratory rate")))

print(Plot$Plot)




# Save the plot to path

map2(paste(.x=Plot$name,'.png'),.y=Plot$Plot,.f = ggsave,height=6,width=10,dpi=400,path=here::here('Plot/Afternoon High/Respiratory Rate'))



# Pulse Rate

data1 <- dat[[2]]



Filter <-(c("PULRAT1", "PULRAT2","PULRAT3","PULRAT5","PULRAT6","PULRAT7","PULRAT9","PULRAT10","PULRAT14","PULRAT15","PULRAT16","PULRAT17","PULRAT18","PULRAT19","PULRAT20","PULRAT22","PULRAT24","PULRAT25","PULRAT27","PULRAT29","PULRAT31","PULRAT33","PULRAT34","PULRAT35","PULRAT36","PULRAT37","PULRAT38","PULRAT39","PULRAT40","PULRAT41","PULRAT42","PULRAT43","PULRAT44","PULRAT46","PULRAT48","PULRAT49","PULRAT50","PULRAT51","PULRAT52","PULRAT53","PULRAT54","PULRAT55","PULRAT56","PULRAT57","PULRAT58","PULRAT59","PULRAT62","PULRAT63","PULRAT64","PULRAT65","PULRAT66","PULRAT69","PULRAT71","PULRAT72","PULRAT74","PULRAT75","PULRAT77","PULRAT79","PULRAT80","PULRAT81","PULRAT82","PULRAT83","PULRAT85","PULRAT86","PULRAT88","PULRAT90"))



# Correct

Plot <- data1 |>
  pivot_longer(-c(1:4)) |>
  group_by(name) |>
  nest() |>
  filter(name %in% Filter) |>
  mutate(Plot = map2(.x = data, .y = name, .f = ~ ggplot(data = .x, aes(x = Colour, y = value, fill = Sex)) +
                       stat_summary(geom = "errorbar", position = position_dodge(.9), fun.data = mean_se,width=0.7,col='black',size=0.7) +
                       stat_summary(geom = "col", position = "dodge", fun = "mean") +
                       theme_light() +
                       scale_fill_brewer(palette = "Dark2") +
                       labs(y = "Pulse rate")))

print(Plot$Plot)




# Save the plot to path

map2(paste(.x=Plot$name,'.png'),.y=Plot$Plot,.f = ggsave,height=6,width=10,dpi=400,path='Plot/Afternoon High/Pulse Rate')


# Rectal Temperature




data1 <- dat[[3]]



Filter <-(c("RECTEM1", "RECTEM2","RECTEM3","RECTEM4","RECTEM5","RECTEM6","RECTEM7","RECTEM8","RECTEM9","RECTEM10","RECTEM11","RECTEM12","RECTEM13","RECTEM14","RECTEM15","RECTEM16","RECTEM18","RECTEM20","RECTEM21","RECTEM24","RECTEM27","RECTEM28","RECTEM29","RECTEM30","RECTEM31","RECTEM34","RECTEM36","RECTEM37","RECTEM38","RECTEM39","RECTEM40","RECTEM41","RECTEM42","RECTEM45","RECTEM46","RECTEM47","RECTEM48","RECTEM50","RECTEM51","RECTEM52","RECTEM54","RECTEM55","RECTEM56","RECTEM57","RECTEM58","RECTEM61","RECTEM62","RECTEM63","RECTEM66","RECTEM68","RECTEM69","RECTEM70","RECTEM71","RECTEM73","RECTEM74","RECTEM75","RECTEM77","RECTEM78","RECTEM79","RECTEM80","RECTEM81","RECTEM83","RECTEM85","RECTEM86","RECTEM89","RECTEM90"))



# Correct

# Rectal Temperature


Plot <- data1 |>
  pivot_longer(-c(1:4)) |>
  group_by(name) |>
  nest() |>
  filter(name %in% Filter) |>
  mutate(Plot = map2(.x = data, .y = name, .f = ~ ggplot(data = .x, aes(x = Colour, y = value, fill = Sex)) +
                       stat_summary(geom = "errorbar", position = position_dodge(.9), fun.data = mean_se,width=0.7,col='black',size=0.7) +
                       stat_summary(geom = "col", position = "dodge", fun = "mean") +
                       theme_light() +
                       scale_fill_brewer(palette = "Dark2") +
                       labs(y = "Rectal temperature")))

print(Plot$Plot)




# Save the plot to path

map2(paste(.x=Plot$name,'.png'),.y=Plot$Plot,.f = ggsave,height=6,width=10,dpi=400,path='Plot/Afternoon High/Rectal Temperature')


# Afternoon Low

