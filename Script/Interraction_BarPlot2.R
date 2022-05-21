#Interraction bar plot(N against Variety) in R
library(psych)
library(ggplot2)

data<-read.csv('Data/split.csv')


data$N<-as.factor(data$N)
data$Maize<-as.factor(data$Maize)

data2<-describeBy(data$YIELD,list(data$N,data$Maize),mat=TRUE,digits=2)

names(data2)[names(data2) =='group1'] = 'N.Rate'

names(data2)[names(data2) =='group2'] = 'Maize'

levels(data2$N.Rate)[levels(data2$N.Rate)=='0'] = '30 kg N'

levels(data2$N.Rate)[levels(data2$N.Rate)=='1'] = '60 kg N'

levels(data2$Maize)[levels(data2$Maize)=='1'] = 'Abeleko'

levels(data2$Maize)[levels(data2$Maize)=='2'] = 'Kewesoke'

levels(data2$Maize)[levels(data2$Maize)=='3'] = 'TZESR'

levels(data2$Maize)[levels(data2$Maize)=='4'] = 'SUWAN 1'

data2$se = data2$sd/sqrt(data2$n)

limits = aes(ymax = mean +(3.0*se),ymin=mean-(3.0*se))

dodge = position_dodge(width=0.9)
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'))
p=ggplot(data2,aes(x = Maize,y = mean,fill = N.Rate))+
  geom_bar(stat='identity',position=dodge)+
  geom_errorbar(limits, position=dodge, width=0.25)+
  apatheme+
  ylab('MEAN YIELD')+
  scale_fill_grey()
p



