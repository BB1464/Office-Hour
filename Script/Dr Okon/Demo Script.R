
############################################################################
############################################################################
###                                                                      ###
###                    BARPLOT WITH SIGNIFICANT STARS                    ###
###                                                                      ###
############################################################################
############################################################################

# Uncomment this line below and run it if you did not have the packages
#install.packages('ggplot2')
#install.packages('ggsignif')


library(ggplot2)
library(ggsignif)

dat <- data.frame(Group = c("S1", "S1", "S2", "S2"),
                  Sub   = c("A", "B", "A", "B"),
                  Value = c(3,5,7,8))

ggplot(dat, aes(Group, Value)) +
  geom_bar(aes(fill = Sub), stat="identity", position="dodge", width=.5) +
  geom_signif(stat="identity",
              data=data.frame(x=c(0.875, 1.875), xend=c(1.125, 2.125),
                              y=c(5.8, 8.5), annotation=c("**", "NS")),
              aes(x=x,xend=xend, y=y, yend=y, annotation=annotation)) +
  geom_signif(comparisons=list(c("S1", "S2")), annotations="***",
              y_position = 9.3, tip_length = 0, vjust=0.4) +
  scale_fill_brewer(palette = 'Dark2')+
  theme_classic()



############################################################################
############################################################################
###                                                                      ###
###                                T TEST                                ###
###                                                                      ###
############################################################################
############################################################################

t.test(mtcars$mpg,mtcars$cyl) # Paired t test between two variable


t.test(mtcars$mpg) # One sample t test



# Second Approach
dat <- data.frame(Group = c("S1", "S1", "S2", "S2"),
                  Sub   = c("A", "B", "A", "B"),
                  Value = c(3,5,7,8))

ggplot(dat, aes(Group, Value)) +
  geom_bar(aes(fill = Sub), stat="identity", position="dodge", width=.5) +
  geom_signif(stat="identity",
              data=data.frame(x=c(0.875, 1.875), xend=c(1.125, 2.125),
                              y=c(5.8, 8.5), annotation=c("**", "NS")),
              aes(x=x,xend=xend, y=y, yend=y, annotation=annotation)) +
  geom_signif(comparisons=list(c("S1", "S2")), annotations="***",
              y_position = 9.3, tip_length = 0, vjust=0.4) +
  scale_fill_manual(values = c("grey80", "grey20"))



# Third Approach
dat <- data.frame(name=c("a","b","a","c","a","b","d","c"),
                  val=c(1,-1.2,2.2,0.9,-0.7,2.4,-2.3,0.6),
                  sd=c(0.2,0.3,0.25,0.2,0.25,0.6,0.56,0.2),
                  p=c(0.09,0.1,0.02,0.1,0.2,0.001,0.001,0.6),
                  grp=c("A","A","B","B","C","C","C","C"))

dat$star <- ""
dat$star[dat$p <= .05]  <- "*"
dat$star[dat$p <= .01]  <- "**"
dat$star[dat$p <= .001] <- "***"

ggplot(dat, aes(x=name,y=val,width=0.5,ymin=val-sd,ymax=val+sd)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(stat="identity", position="dodge") +
  geom_text(aes(label=star), colour="red", vjust=0, size=10) +
  facet_grid(. ~ grp, scales="free_x", space="free",margin=FALSE)
