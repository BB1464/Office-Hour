############################################################################
############################################################################
###                                                                      ###
###                        DADA ANALYSIS PIPELINE                        ###
###                                                                      ###
############################################################################
############################################################################

library(tidyverse)
library(readxl)
library(factoextra)
library(ggsci)


# Import the data set into R

data <- read_excel(path = here::here('Data/Copy of PHASEOLUS 5TH WEEK DATA ES8.xls'))


# second data set import
data5 <- read_excel(path = here::here('Data/Copy of Phaseolus Dry Weight Data.xls'))

data5 <- data5 %>% select(2,3,11:13,15) %>% drop_na() %>% mutate(across(.cols = c(1),.fns = factor))

dat6 <- data5 %>% group_by(accn) %>% summarise_all(.funs = mean)

#dat7 <- dat6 %>% column_to_rownames(var = 'accn')

# First Pipeline

dat <- data %>% pivot_longer(cols = c(15:19),names_to = 'Week',values_to = 'Leaf rachis Length') %>% pivot_longer(cols = c(15:19),names_to = 'WK',values_to = 'Petiole Leght') %>% pivot_longer(cols = c(15:19),names_to = 'WK1',values_to = 'Terminal Petiole Length') %>% pivot_longer(cols = c(15:19),names_to = 'wk',values_to = 'Terminal leaflet Length') %>% pivot_longer(cols = c(15:19),names_to = 'wks1',values_to = 'Terminal leaflet Width') %>% pivot_longer(cols = c(15:19),names_to = 'wks11',values_to = 'Plant Height') %>% pivot_longer(cols = c(15:17),names_to = 'wks12',values_to = 'Leaf Chlorophyll Content') %>% select(2,3,5,15,16,18,20,22,24,26,28)




dat2 <- dat %>% select(-c(2:4)) %>% mutate(across(.cols = c(1),.fns = factor))

dat2 <- dat2 %>% drop_na()

dat3 <- dat2 %>% group_by(accn) %>% summarise_all(.funs = mean)


dat8 <- dat6 %>% inner_join(dat3,by = 'accn') # Join the two data frame together

dat8 <- dat8 %>% rename('Shoot dry weight'='Dry Weight Shoot 1','Root dry weight'='Dry Weight Root 1','No of Noodles per Plant'='No Of Nodules Per Plant 1','Petiole Length'='Petiole Leght') %>% select(-Rep)


dat4 <-  dat8 %>% column_to_rownames(var = 'accn')

write.csv(x = dat4,here::here('Data/data_PCA.csv'))

# Cluster analysis

scale <- scale(x = dat4)

# Compute the dissimilarity matrix
DT <- dist(x = scale,method = 'euclidean')


#Visualize the distance matrix using a heatmap
fviz_dist(dist.obj = DT) # Heat map
# Save the output
ggsave(path = here::here('Plot'),'heatmap.png',width = 10,height = 7,dpi = 400)


# Visualize the number of clusters
fviz_nbclust(x = scale,kmeans,method = 'wss')


# Perform the hierarchical clusters

Clusters <- hclust(d = DT,method = 'ward.D')


# Visualize the clusters using a dendogram

fviz_dend(x = Clusters,k = 4,k_colors = 'aaas')+
  theme_void()

ggsave(path = here::here('Plot'),'dendro.png',width = 10,height = 7,dpi = 400)

# Perform the Principal Component Analysis

pca <- dat8 %>% select(where(is.numeric)) %>%
  scale() %>%
  prcomp()

# Get the Eigene Values

get_eig(X = pca)
fviz_eig(X = pca,addlabels = TRUE)

ggsave(path = here::here('Plot'),'screeplot.png',width = 10,height = 7,dpi = 400)


fviz_pca_biplot(X = pca)

# PCA Output
ggsave(path = here::here('Plot'),'PCA.png',width = 10,height = 7,dpi = 400)


# Second Approach for PCA with names of the accession
# Perform the Principal Component Analysis

pca <- dat4 %>% select(where(is.numeric)) %>%
  scale() %>%
  prcomp()

# Get the Eigene Values

#get_eig(X = pca)
#fviz_eig(X = pca,addlabels = TRUE)

fviz_pca_biplot(X = pca,repel = TRUE)

# PCA Output
ggsave(path = here::here('Plot'),'PCA2.png',width = 10,height = 7,dpi = 400)

