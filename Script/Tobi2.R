############################################################################
############################################################################
###                                                                      ###
###                         ALPHA LATTICE DESIGN                         ###
###                                                                      ###
############################################################################
############################################################################


## Import the necessary library

library(tidyverse)
library(lmerTest)
library(inti)
library(emmeans)
library(readxl)

# Import the dataset into R
# Average
dat <- read_excel(path=here::here('~/../Desktop/REANALYSED DATA.xlsx'),sheet = 'AVERAGE')



dat <- read_excel(path = here::here('Data/REANALYSED DATA.xlsx'),sheet = 'RAW DATA') |> filter(name!='TPv-694')

# Data Wrangling Step
###
###

dat_clean <- dat |> filter(if_all(.cols = everything(),.fns = ~!is.na(.)))


# Convert Character to factor

dat_clean <- dat_clean |> mutate(across(.cols = c(2:5),.fns = factor))

# Check the structure of the data

glimpse(dat_clean)


# Convert the dataset for all the trait into a long format

dat_long <- dat_clean |>
  pivot_longer(cols = c(6:20),names_repair = 'minimal') |>
  rename(Genotype='name') |>
  filter(Genotype!='TPv-694') |>
  group_by(name) |>
  nest()



# Create a function for all the model

model_fun <- function(df,name){
  model <- lmer(formula = value~Genotype+(1|Rep)+(1|Rep:block),data = df)
}



# Fit the model for all the trait at once using purrr

mod <- dat_long |>
  mutate(model=map2(.x = data,.y = name,.f = model_fun)) |>
  mutate(anova=map(.x = model,.f = anova)) |>
  mutate(Post_Hoc=map(.x = model,.f = ~emmeans(object = .x,specs =pairwise ~Genotype))) |>
select(anova,Post_Hoc)


## Extract the anova table for all the model
mod |> pluck('anova') |>
  set_names(mod$name)

## Extract the means to show which Genotype is significantly different
mod |>
  pluck('Post_Hoc') |>
  set_names(mod$name)


## Get the BLUPS from the model

library(inti)

dt <- dat_long






MOD <- function(df,name){

hr <- H2cal(data = df
              , trait = "value"
              , gen.name = "Genotype"
              , rep.n = 5
              , fixed.model = "1 + (1|block) + Genotype"
              , random.model = "0 + (1|block) + (1|Genotype)"
              , emmeans = TRUE
              , plot_diag = FALSE
              , outliers.rm = TRUE
  )
return(hr)
}



# Get the output from inti
Blubs <- dt |>
  mutate(res=map2(.x = data,.y = name,.f = MOD)) |>
  select(-data)

# Extract the blubs for Terminal length
Blubs$res[[1]]$blups

# Extract the blubs for Terminal width
Blubs$res[[2]]$blups

# Extract the blubs for Petiole length
Blubs$res[[3]]$blups

# Extract the blubs for Leaf Rachis
Blubs$res[[4]]$blups

# Extract the blubs for Plant Height
Blubs$res[[5]]$blups

# Extract the blubs for Number of Branchis
Blubs$res[[6]]$blups

# Extract the blubs for Pod Weight
Blubs$res[[7]]$blups

# Extract the blubs for Number of Pods
Blubs$res[[8]]$blups

# Extract the blubs for Pod Length
Blubs$res[[9]]$blups

# Extract the blubs for Pod Width
Blubs$res[[10]]$blups

# Extract the blubs for Pod Length
Blubs$res[[11]]$blups

# Extract the blubs for Seed Weight
Blubs$res[[12]]$blups

# Extract the blubs for Seed Length
Blubs$res[[13]]$blups


# Extract the blubs for Seed Width
Blubs$res[[14]]$blups

# Extract the blubs for Seed Thickness
Blubs$res[[15]]$blups


############################################################################
############################################################################
###                                                                      ###
###                           CLUSTER ANALYSIS                           ###
###                                                                      ###
############################################################################
############################################################################

cluster_dat <- dat_clean |> select(-c(sn,barcode,Rep,block)) |>
  group_by(name) |>
  filter(name!='TPv-694') |>
  summarise_all(.funs = mean) |>
  column_to_rownames(var = 'name')


DIST <- dist(x = cluster_dat,method = 'euclidean')


Cluster <- hclust(d = DIST,method = 'ward.D')

## Plot the cluster using base R Function

plot(Cluster)


### Advance plotting using factoextra and ggplot2

library(factoextra)
library(ggsci)
library(MetBrewer)

fviz_dend(x = Cluster,k = 4,k_colors = 'lancet',lwd = 0.8,cex = 1)+
  theme_void()+
  theme(plot.title = element_text(family = 'serif',face = 'bold',size = 14,hjust = 0.5),text = element_text(family = 'serif',face = 'bold',size = 12))

## Save the output

ggsave('Cluster_Dendogram_Tobi.png',path = here::here('Plot'),width = 10,height = 10,dpi = 320,bg = 'white')


############################################################################
############################################################################
###                                                                      ###
###                     PRINCIPAL COMPONENT ANALYSIS                     ###
###                                                                      ###
############################################################################
############################################################################


Pca_dat <- dat_clean |> select(where(is.numeric)) |>
  select(-1) |>
  rename(PL=`POD LENGTH`,PTL=`PETIOLE LENGTH`,LR=`LEAF RACHIS`,SL=`SEED LENGTH`,LPP=`LOCULE PER POD`,TW=`terminal width`,NB=`NUMBER OF BRANCHES`,PW=`POD WEIGHT`,SW=`SEED WEIGHT`,NP=`NO OF PODS`,PH=`PLANT HEIGHT @6WEEKS`,SOW=`SEED WIDTH`,POW=`POD WIDTH`,ST=`SEED THICKNESS`,TL=`Terminal length`)

## Principal Component Function

PCA <- prcomp(x = Pca_dat,scale=TRUE,center=TRUE)

summary(PCA)

# Get all the eigen value
get_eigenvalue(X = PCA)

# Visualuaze the number of cluster
fviz_screeplot(PCA)


fviz_pca_biplot(X = PCA)+
  theme(text = element_text(family = 'serif',face = 'bold',size = 14,colour = 'black'),axis.title = element_text(family = 'serif',face = 'bold',size = 14,colour = 'black'),axis.text = element_text(family = 'serif',face = 'bold',size = 14,colour = 'black'))

### Save the output
###


ggsave('Cluster_PCA_Tobi.png',path = here::here('Plot'),width = 10,height = 10,dpi = 320,bg = 'white')

