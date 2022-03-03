
############################################################################
############################################################################
###                                                                      ###
###      PRINCIPAL COMPONENT ANALYSIS VISUALIZATION WITH FACTOEXTRA      ###
###                                                                      ###
############################################################################
############################################################################


library(factoextra)
data("iris")

res.pca <- prcomp(iris[,-5],scale = TRUE)

fviz_pca_ind(res.pca,axes = c(1,2),
             label='var',col.var='black',geom='point',pointsize=3,col.ind = iris$Species,addEllipses = TRUE,ellipse.level=0.95,ellipse.type='confidence',palette = 'aaas')+theme_minimal()#+
  #scale_shape_manual(values = c(19,20,21))


