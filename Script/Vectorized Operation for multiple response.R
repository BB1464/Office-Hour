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
response

expl <- names(mtcars)[5:7]
expl

response <- set_names(response)
response

expl <- set_names(expl)
expl

plot=function(x,y){
  ggplot(data = mtcars,aes(x = .data[[x]],y = .data[[y]]))+
    geom_point()+geom_smooth()
}

# Create plot

plot(x = 'wt',y = 'mpg')



# Looping through one vector

elev_plot <- map(.x = expl,.f = ~plot(x = .x,y ='mpg'))
elev_plot

# Looping through both Vector

all_plot <- map(.x = response,.f = ~map(.x = expl,.f = plot,y=.x))
all_plot

# Create a plot grid

resp_expl <- tidyr::expand_grid(expl,response)
resp_expl


all_plot2 <- pmap(.l = resp_expl,.f = ~plot(x = .x,y = .y))
all_plot2

# Saving the Output with names
all_plot2_names <- pmap(.l = resp_expl,.f = ~paste0(.x,'-',.y,'.png'))


# Saving the Plot
pdf('all_scatterplots.pdf')


# Saving group of plot together
iwalk(all_plot2,~{
  png(paste0(.y,'scatterplots.png'))
  print(.x)
  dev.off()
})

