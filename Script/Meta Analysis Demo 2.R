
###########################################################################
###########################################################################
###                                                                     ###
###                            META ANALYSIS                            ###
###                                                                     ###
###########################################################################
###########################################################################

library(metafor)
library(metaviz)
library(ggplot2)

dat <- dat.bcg

# Effect size computation

es <- escalc(measure = 'RR',ai = tpos,bi = tneg,ci = cpos,di = cneg,data=dat)


# Fit the random effect model using the rma.mv function

mod <- rma.mv(yi = yi,V = vi,random = ~author|alloc,data=es)


# Visualize the forest plot

forest(mod,slab = paste0(es$author,',',es$year,sep=''),header = TRUE)

# Let me use meta_viz forest function for the visualization

viz_forest(mod,study_labels = paste0(es$author,',',es$year,sep=''))+
  theme_classic()+
  ggeasy::easy_remove_legend()


# Funnel plot for publication bias

funnel(mod)

viz_funnel(mod)


