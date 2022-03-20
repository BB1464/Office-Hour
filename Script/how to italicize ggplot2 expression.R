
# Data frame

df <- data.frame(x=c(1:5), y=c(1:5))
a <- 1
b <- 2
r2 <- 0.9
eq <- paste("y = ", a, " + ", b, "(x); r^2=", r2)
ggplot(data=df, aes(x=x, y=y))+
  geom_point(color="black")+
  geom_text(x=2, y=4,label=eq, parse=FALSE)


###########################################################################
###########################################################################
###                                                                     ###
###              HOW TO ITALICIZE AN EXPRESSION IN GGPLOT2              ###
###                                                                     ###
###########################################################################
###########################################################################


library(ggplot2)
ggplot(data=df, aes(x=x, y=y)) +
  geom_point(color="black") +
  annotate('text', 2.5, 4,
           label=paste("italic(y)==", a, "+", b,
                       "~italic(x)~';'~italic(r)^2==", r2),
           parse=TRUE,
           hjust=1, size=5)
