

library(readxl)
library(tidyverse)

# Import the dataset
# data_2021 <- read_excel(path = here::here('Data/100 Seed WGHT Combined 2021.xlsx'),sheet = 'All env 2021',na = '.')


data_2021 <- read_excel(path=here::here('Data/Ben 100 Seed WGHT Combined 2021.xlsx'),sheet = 'All env 2021',na = c('.',''))


# Drop the missing data
data_clean <- data_2021 %>%
  drop_na()

# Pivoted dataset
dat <- data_clean %>%
  pivot_longer(cols = c(8:22)) %>%
  group_by(name) %>%
  nest()



# Plotting Function
plot_function <- function(df,y){
ggplot(data = df,aes(x = Accession,y = value,fill=WaterRegimes))+
    stat_boxplot(geom = 'errorbar')+
    geom_boxplot(outlier.shape = NA)+
    facet_grid(~Location)+labs(y=y)+
    scale_y_log10()+ # Place the y axis on a log scale
    coord_flip()+
    theme_test()+
    scale_fill_brewer(palette = 'Set1')+
    theme(strip.text = element_text(family = 'serif',face = 'bold',size = 18,colour = 'black'),
          #axis.text.y = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
          axis.title.y = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
          axis.title.x = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
          axis.text.x = element_text(family = 'serif',face = 'bold',colour = 'black',size=18),
          legend.title = element_text(family = 'serif',face = 'bold',colour = 'black',size = 22),
          legend.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18))
}


# Plot the graph for three way Interraction
plot <- dat %>%
  mutate(plot=map2(.x = data,.y = name,.f = plot_function)) %>%
  select(plot)



# Save the plot
map2(paste(plot$name,'.png'),plot$plot,
     .f = ggsave,path=here::here('Script/Accession_Loc_Regime_2021'),width=20,height=16,dpi=370)




# Accesion by Location

# Plotting Function

plot_function <- function(df,y){
  ggplot(data = df,aes(x = Accession,y = value,fill=Location))+
    stat_boxplot(geom = 'errorbar')+
    geom_boxplot(outlier.shape = NA)+
    scale_y_log10()+
    labs(y=y)+
    coord_flip()+
    theme_test()+
    scale_fill_brewer(palette = 'Set1')+
    theme(strip.text = element_text(family = 'serif',face = 'bold',size = 18,colour = 'black'),
          #axis.text.y = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
          axis.title.y = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
          axis.title.x = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
          axis.text.x = element_text(family = 'serif',face = 'bold',colour = 'black',size=18),
          legend.title = element_text(family = 'serif',face = 'bold',colour = 'black',size = 22),
          legend.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18))
}


# Plot the graph
plot <- dat %>%
  mutate(plot=map2(.x = data,.y = name,.f = plot_function)) %>%
  select(plot)





# Save the plot
map2(paste(plot$name,'.png'),plot$plot,
     .f = ggsave,path=here::here('Script/Accession_Location_2021'),width=20,height=16,dpi=370)


# Accession by WaterRegime

# Plotting Function

plot_function <- function(df,y){
  ggplot(data = df,aes(x = Accession,y = value,fill=WaterRegimes))+
    stat_boxplot(geom = 'errorbar')+
    geom_boxplot(outlier.shape = NA)+
    labs(y=y)+
    scale_y_log10()+
    coord_flip()+
    theme_test()+
    scale_fill_brewer(palette = 'Set1')+
    theme(strip.text = element_text(family = 'serif',face = 'bold',size = 18,colour = 'black'),
          #axis.text.y = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
          axis.title.y = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
          axis.title.x = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
          axis.text.x = element_text(family = 'serif',face = 'bold',colour = 'black',size=18),
          legend.title = element_text(family = 'serif',face = 'bold',colour = 'black',size = 22),
          legend.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18))
}


# Plot the graph
plot <- dat %>%
  mutate(plot=map2(.x = data,.y = name,.f = plot_function)) %>%
  select(plot)





# Save the plot
map2(paste(plot$name,'.png'),plot$plot,
     .f = ggsave,path=here::here('Script/Accession_WaterRegimes_2021'),width=20,height=16,dpi=370)




# Location by WaterRegime Continue tommorow

# stat_summary(geom = 'errorbar',position = 'dodge',fun.data = 'mean_se')+stat_summary(geom = 'col',fun = 'mean',position = position_dodge(.9))+labs(y=y)+scale_y_log10()

plot_function <- function(df,y){
ggplot(data = df,aes(x =  Location,y =  value,fill= WaterRegimes))+
stat_summary(geom = 'errorbar',fun.data = 'mean_se',position = 'dodge')+stat_summary(geom = 'col',fun = 'mean',position = position_dodge(.9))+labs(y=y)+
theme_test()+scale_y_log10()+
scale_fill_brewer(palette = 'Set1')+
theme(axis.text.y = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
          axis.title.y = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
          axis.title.x = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18),
          axis.text.x = element_text(family = 'serif',face = 'bold',colour = 'black',size=18),
          legend.title = element_text(family = 'serif',face = 'bold',colour = 'black',size = 22),
          legend.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 18))
}


# Plot the graph
plot <- dat %>%
  mutate(plot=map2(.x = data,.y = name,.f = plot_function))




# Save the plot
map2(paste(plot$name,'.png'),plot$plot,
     .f = ggsave,path=here::here('Script/Location_WaterRegimes_2021'),width=20,height=16,dpi=370)



