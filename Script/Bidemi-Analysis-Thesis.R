

# Load the Required Packages ----------------------------------------------

library(tidyverse)
library(agricolae)
library(readxl)
library(MetBrewer)
library(report)

# Import the dataset ------------------------------------------------------

dat <- read_excel(path = here::here('Data/Thesis Final DATA.xlsx'))


# Convert Characters to Factor --------------------------------------------

dat <- dat |>
  mutate(across(.cols = c(1:3),.fns = factor))


# Fit the Model -------------------------------------------


for (i in 4:length(dat)) {
  print(paste('----',names(dat[i]),'----'))

model <- lm(dat[[i]]~REP+ACCESSION*NUTRIENT,data=dat)

print(anova(model))
print(report(anova(model)))
print(cv.model(model))

print(HSD.test(y = model,trt = 'ACCESSION',console = TRUE))
print(HSD.test(y = model,trt = 'NUTRIENT',console = TRUE))
print(HSD.test(y = model,trt = c('ACCESSION','NUTRIENT'),console = TRUE))

}




# Interraction Plot -------------------------------------------------------


plot <- dat |>
drop_na() |>
mutate(NUTRIENT=factor(NUTRIENT,levels = c('1/2 N','N','ALL'))) |>
  pivot_longer(cols = c(4:9)) |>
  group_by(name) |>
  nest() |>
  mutate(plot=map2(.x = data,.y = name,.f = ~ggplot(data = .x,aes(x = ACCESSION,y = value,fill=NUTRIENT))+
stat_summary(geom = "errorbar", position = position_dodge(.9), fun.data = mean_se,width=0.7,col='black',size=0.7) +stat_summary(geom = "col", position = "dodge", fun = "mean")+theme_light()+scale_fill_met_d(name = 'Hokusai1')+
scale_y_continuous(expand = expansion(mult = c(0,.1)))+
labs(y = .y,x='Accession',fill='Nutrient')+  theme(axis.text.y = element_text(family = 'serif',colour = 'black',face = 'bold',size = 15),panel.background = element_rect(colour = 'white'),panel.grid = element_blank(),axis.text.x = element_text(family = 'serif',colour = 'black',face = 'bold',size = 14),axis.title = element_text(family = 'serif',colour = 'black',face = 'bold',size = 15),legend.title = element_text(family = 'serif',colour = 'black',face = 'bold',size = 15),legend.text = element_text(family = 'serif',colour = 'black',face = 'bold',size = 15),strip.text = element_text(family = 'serif',colour = 'black',face = 'bold',size = 15))
  ))


print(plot$plot)



# Save the plot to path

map2(paste(.x=plot$name,'.png'),.y=plot$plot,.f = ggsave,height=6,width=13,dpi=400,path=here::here('Plot/Bidemi/Interraction_Plot'))

