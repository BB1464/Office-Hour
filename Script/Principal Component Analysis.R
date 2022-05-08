
###########################################################################
###########################################################################
###                                                                     ###
###                    PRINCIPAL COMPONENTS ANALYSIS                    ###
###                                                                     ###
###########################################################################
###########################################################################

#################################################################
##                  Load the Required Library                  ##
#################################################################

library(tidyverse)
library(factoextra)

pca <- iris %>% select(where(is.numeric)) %>%
  prcomp()

# View the result
pca


get_eigenvalue(pca)

get_eig(pca)

fviz_eig(pca,addlabels = TRUE,ylim=c(0,100))

fviz_pca_ind(pca)
fviz_pca_biplot(pca)

fviz_pca_var(pca)
get_pca_var(pca)

