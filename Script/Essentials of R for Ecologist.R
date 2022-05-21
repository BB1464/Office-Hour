##########################################################
##### The essentials of R (for ecology) Cheat Sheet #######
##########################################################

num_vec <- c(3,6,3,8)
spp_vec <- c("spp1","spp3","spp2","spp3")
dataframe <- data.frame(num_vec, spp_vec)
data(trees)
tree_data <- trees
tree_data$light <- c(rep(c("shade","sun"), each=15),"sun")
tree_data$light <- as.factor(tree_data$light)
my_matrix <- as.matrix(dataframe)

c()

sum(c(num_vec, NA), na.rm=T)

length(dataframe)

length(unique(spp_vec))

char_vec <- c("2","5","1","1")
char_vec <- c(3,5,2,1,5,"O")

as.numeric(char_vec)

log(num_vec)

sort(num_vec, decreasing = T)

sort(spp_vec)
sort(c("d","s","a"))

is.na(c(1,3,5,NA))

new_vec <- c(1,3,5,NA)

#new_vec == NA

new_vec[!is.na(new_vec)]

# %in%

spp_vec %in% c("spp1","spp4")

c("spp1","spp4") %in% spp_vec

#install.packages("lubridate")
library("lubridate")

my_date <- ymd("2016/june, 13")

class(my_date)

dmy("13th of June, 2016")
ydm()

seq(from = 0, to = 10, by = 0.5)

rep(x = 1:3, times = 2)
rep(x = 1:3, each = 2)

grepl("sp", spp_vec)

dataframe[grepl("3", dataframe$spp_vec),]

write.csv(dataframe, "tutorial_scripts/dataframe.csv", row.names=F)

getwd()

dataframe_uploaded <- read.csv("tutorial_scripts/dataframe.csv")

setwd()

my_func <- function(x){
  x_mod <- (x + 5) * 3
  return(x_mod)
}

fm <- function(x) {
  mod<-x+2*5
  return(mod)
}
my_func(num_vec)

help(trees)

data(Titanic)

my_titanic_data <- Titanic


### SIDE 2:

plot(Height ~ Volume, data=tree_data)
plot(Height ~ light, data=tree_data)

# ~ = "is a function of"
# whatever is on the left of the tilde is the Y variable
# or it's what will appear on the y axis

hist(tree_data$Height)


fitted_model <- lm(Height ~ Volume, data=tree_data)
plot(Height ~ Volume, data=tree_data)
abline(fitted_model)
abline(v = 40)
abline(h = 85)

# Y = a + bX

### Working with data frames

names(dataframe)

names(tree_data)[1] <- "DBH"

data.frame()

num_vec <- c(3,6,3,8)
spp_vec <- c("spp1","spp3","spp2","spp3")
dataframe <- data.frame(num_vec, spp_vec)

library(dplyr)
my_tibble <- tibble(num_vec, spp_vec)

my_tree_tibble <- as_tibble(tree_data)
tree_data

my_matrix <- as.matrix(dataframe)

as.data.frame(my_matrix)

as_tibble()

t(my_matrix)

ncol(tree_data)
nrow(tree_data)

head(tree_data)

# uses dplyr package
left_join()

my_tree_tibble$tree_ID <- as.character(1:31)

my_treeleaf_data <- tibble(tree_ID = as.character(1:31), tree_leaves = round(runif(n=31, min=3000, max=5000)))

my_treeleaf_data <- my_treeleaf_data[sample(1:31),]

my_data_all <- left_join(my_tree_tibble, my_treeleaf_data, by="tree_ID")

library("dplyr")

select(my_data_all, leaf_number = tree_leaves)

filter(my_data_all, DBH < 11 | Height >= 70)

mutate(my_data_all,
       Height = Height * 0.3048)

summarize(my_data_all,
          mean_height = mean(Height),
          max_DBH = max(DBH))

my_data_grouped <- group_by(my_data_all, light)

summarize(my_data_grouped,
          mean_height = mean(Height),
          max_DBH = max(DBH))

ungroup(my_data_grouped)

mod1 <- lm(DBH ~ light + tree_leaves, data=my_data_all)
summary(mod1)

mean(num_vec, na.rm=T)
max(my_data_all$Height)
min(my_data_all$Height)
median(my_data_all$Height)
mean(my_data_all$Height)

table(my_data_all$light)







