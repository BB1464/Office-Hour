###########################################################################
###########################################################################
###                                                                     ###
###                      EXPLORATORY DATA ANALYSIS                      ###
###                                                                     ###
###########################################################################
###########################################################################


library(tidyverse)
library(agricolae)
library(readxl)


# List of all the sheet
excel_sheets(path = here::here('Data/Mrs_Robinson2.xlsx'))


# Read in the dataset into R as a list
dat_list <- excel_sheets(path = here::here('Data/Mrs_Robinson2.xlsx')) |>
  map(.f = ~read_excel(path = here::here('Data/Mrs_Robinson2.xlsx'),sheet = .,na = '*'))


# Import The Proximate dataset into R

proximate <- dat_list[[13]]

proximate_clean <- proximate |> mutate(across(.cols = c(1:3),.fns = factor)) |>fill(Treatment) |> janitor::clean_names()


for (i in 4:ncol(proximate_clean)) {
  print(paste('---',names(proximate_clean[i]),'---'))

model <- lm(formula = proximate_clean[[i]]~rep+treatment*variety,data=proximate_clean)

print(anova(model))
print(cv.model(model))

lala=HSD.test(y = model,trt = 'treatment',console = TRUE)
lala=HSD.test(y = model,trt = 'variety',console = TRUE)
lala=HSD.test(y = model,trt = c('treatment','variety'),console = TRUE)

print(lala)

}


# Visualize the interraction plot for all the trait


plot <- proximate_clean |>
  rename(`% Organic Carbon`='percent_organic_carbon',`Total nitrogen`='percent_nitrogen',`Available P`='percent_p',Potassium='percent_k',Sodium='percent_na',Sulphur='sulphur') |>
  pivot_longer(cols = c(4:9)) |>
  select(-rep) |>
  group_by(name) |>
  nest() |>
  mutate(plot=map2(.x = data,.y = name,.f = function(x,y){
    ggplot(data = x,aes(x = treatment,y = value,fill=variety))+
      stat_summary(geom = 'errorbar',position = 'dodge',fun.data = mean_se)+
      stat_summary(geom = 'col',position = position_dodge(.9),fun = mean)+
      labs(y=y)+
      theme_classic()+
      scale_fill_brewer(palette = 'Set1')+
      theme(text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12),axis.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 12))+
  coord_cartesian(expand = FALSE,clip = 'off')
  }))




plot2 <- plot |> select(-data) |>
  walk(print)



# Save the plot for Screen House Proximate Analysis
map2(paste(plot2$name,'.png'),.y = plot2$plot,.f = ggsave,path=here::here('Plot'),width=10,height=10,dpi=400,bg='white')
