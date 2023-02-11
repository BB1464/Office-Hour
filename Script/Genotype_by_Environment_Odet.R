

library(tidyverse)
library(metan)



# Import the dataset into R -----------------------------------------------

Hybrid <- read_csv(file = here::here('Data/Combined DADA BA22364 Drought and well watered.csv'))


Inbred <- read_csv(file = here::here('Data/Combined well watered and drought BA22365.csv'))


## Inspect the dataset
inspect(Hybrid)

inspect(Inbred)


# ## Get the experimental details
# ge_details(dat1,
#            env = ENV,
#            gen = Pedigree,
#            resp = everything())
#
#
# ### Visualize the result
#
# ge_plot(dat1, Pedigree, ENV, GY)
#
#
# ### Genotype by Environment Means
# ###
#
# mge <- ge_means(dat1,
#                 env = ENV,
#                 gen = Pedigree,
#                 resp = everything())
#
#
# get_model_data(mge) %>% round_cols()
#


### Genotype and Genotype by Environment Model
###


gge_model <- gge(Hybrid, ENV, Entry, GY)


theme_set(theme_bw(base_size = 20))

plot(x = gge_model,type = 3,line.type.gen = 1,size.shape.win = 3,col.line = 'black',col.gen = 'black',col.env = 'red',axis_expand = 1.5) ## Which one Where



## Save the Output for grain yield
ggsave('GY_Hybrid.png',width = 15,height = 13,dpi = 150,path = here::here('Plot'))



## Inbred
##
gge_model <- gge(Inbred, ENV, Entry, GY)


plot(x = gge_model,type = 3,line.type.gen = 1,size.shape.win = 3,col.line = 'black',col.gen = 'black',col.env = 'red') ## Which one Where



## Save the Output for grain yield
ggsave('GY_Inbred.png',width = 15,height = 13,dpi = 400,path = here::here('Plot'))
