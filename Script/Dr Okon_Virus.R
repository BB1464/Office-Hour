

library(tidyverse)
library(readxl)
library(MetBrewer)

dat <- read_excel(path = here::here('Data/Okon_virus detection.xlsx'))



dat |> janitor::clean_names() |> mutate(across(.cols = everything(),.fns = factor)) |> pivot_longer(cols = c(stem_blot,upper_leaf)) |>
  group_by(genotype,name,score=factor(value)) |>
  count() |>
  ggplot(aes(x = genotype,y = n,fill=fct_reorder2(.f = score,.x = genotype,.y = n)))+
  geom_col(position = position_dodge(width = .9))+
scale_fill_brewer(name='',palette = 'Dark2',label=c('Resistance','Susceptible'))+
  #scale_fill_manual(name='',values = c('#D4D4D4','#737373'),label=c('Resistance','Susceptible')) +
  facet_grid(~name) +
geom_text(aes(label=n),position=position_dodge(.9),vjust=0.1,color='black',fontface='bold')+
  scale_x_discrete(label=c('Heterozygous','Resistance','Susceptible'))+
theme_classic()+
  labs(y='count')+
scale_y_continuous(expand = expansion(mult = c(0,.2)))+
theme(panel.background = element_rect(fill = NA,colour = 'black'),axis.text.x = element_text(family = 'serif',size = 14,face = 'bold',colour = 'black'),axis.title.x = element_text(family = 'serif',face = 'bold',colour = 'black',size = 14),axis.text.y = element_text(family = 'serif',size = 14,face = 'bold',colour = 'black'),axis.title.y = element_text(family = 'serif',face = 'bold',colour = 'black',size = 14),legend.text = element_text(family = 'serif',face = 'bold',colour = 'black',size = 14),strip.text.x = element_text(family = 'serif',face = 'bold',colour = 'black',size = 14))



# Save the plot

ggsave('virus_detection.png',path = here::here('Plot'),width = 10,height = 10,dpi = 400,bg = 'white')
