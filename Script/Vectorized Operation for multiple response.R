############################################################################
############################################################################
###                                                                      ###
###              VECTORIZED OPERATION FOR MULTIPLE RESPONSE              ###
###                                                                      ###
############################################################################
############################################################################





library(tidyverse)
library(broom)
library(cowplot)

head(mtcars)

response <- names(mtcars)[1:4]
expl <- names(mtcars)[5:7]

response <- set_names(response)
expl <- set_names(expl)

plot=function(x,y){
  ggplot(data = mtcars,aes(x = .data[[x]],y = .data[[y]]))+
    geom_point()+geom_smooth()
}

# Create plot

plot(x = 'wt',y = 'mpg')



# Looping through one vector

elev_plot <- map(.x = expl,.f = ~plot(x = .x,y ='mpg'))


# Looping through both Vector

all_plot <- map(.x = response,.f = ~map(.x = expl,.f = plot,y=.x))

# Create a plot grid

resp_expl <- tidyr::expand_grid(expl,response)


all_plot2 <- pmap(.l = resp_expl,.f = ~plot(x = .x,y = .y))


