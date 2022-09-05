############################################################################
############################################################################
###                                                                      ###
###                  THIS IS THE ANALYSIS OF SPLIT PLOT                  ###
###                                                                      ###
############################################################################
############################################################################

library(tidyverse)
library(agricolae)


?sp.plot()
data(plots)
head(plots)

model <- with(plots,sp.plot(block,A,B,yield))

# Alternatively
model <- with(sp.plot(block = block,pplot = A,splot = B,Y = yield),data=plots)

gla=model$gl.a
glb=model$gl.b
Ea=model$Ea
Eb=model$Eb

out1<-with(plots,LSD.test(y=yield,A,gla,Ea,console = TRUE))

out2<-with(plots,LSD.test(y=yield,B,glb,Eb,console = TRUE))

