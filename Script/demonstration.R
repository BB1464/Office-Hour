library(ggplot2)
ggplot(Regression_analysis_SK2,aes(x = Wateruseefficiency, y = Fruityield)) +
  geom_point() +
  geom_smooth(method='lm', se = FALSE, color = 'turquoise4') +
  theme_classic() +
  labs(x='Water use efficiency (kg m "^3")', y= 'Fruit yield (g/plant)')

# The part I have been struggling with is the unit for Water use efficiency  (kg/m3) which I have not been able to get with the R codes I have been trying. The Y axis label fruit yield could also be better written as  g-plant instead of g/plant.

g <- ggplot(iris, aes(Species, Sepal.Length)) + geom_boxplot()
g + xlab(bquote('Superscript as a literal' ~~ Ca^'2+'))

ggplot(data = mtcars,aes(x = mpg,y = cyl))+
  geom_point()+
  geom_smooth(method = 'lm',se=FALSE)+
  theme_classic()+
  labs(x=bquote('Water use efficiency' ~~(kg/m^'3')),y=bquote('Fruit yield' ~~(g^'plant')))



library(ggplot2)
ggplot(Regression_analysis_SK2,aes(x = Wateruseefficiency, y = Fruityield)) +
  geom_point() +
  geom_smooth(method='lm', se = FALSE, color = 'turquoise4') +
  theme_classic() +
  labs(x=bquote('Water use efficiency' ~~(kg/m^'3')), y= bquote('Fruit yield'~~ (g ^'plant')))


labs(x='Water use efficiency (kg/m^3^)',y='Fruit yield (g/plant)')+
  theme(axis.text = element_markdown())
