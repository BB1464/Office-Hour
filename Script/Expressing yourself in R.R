data <- data.frame(Height=c(20,30,50),Weight=c(20,70,90))

library(tidyverse)
head(data) %>% as_tibble()


 data(iris) # This load the iris data set into R
head(iris) # This shows the first six rows
 print(iris) # Print will print all values if it is a data frame but if it is a tibble print will only show you the first 10 rows and it will also show more information of the columns that you read in

 iris %>% as_tibble()


 # tidyr
 ?tidyr
 # separate function
 # unite function
 # pivot_longer
 # pivot_wider


 # Sepaate
 tab <- separate(data = table3,col = 'rate',into = c('Cases','Population'),sep = '/')

 unite(data =tab,col = 'Merge',c('year','Cases'),sep = '/')

 data("billboard")
View(billboard)

# Pivot_Longer
pivot_longer(data = billboard,cols = c('wk1':'wk76'),names_to = 'Week',values_to = 'PH',values_drop_na = TRUE) # The values_drop_na remove all the missing data




# dplyr
?dplyr

# Select
# filter
# mutate and Transmute
# group_by
# Summarize
# Arrange
head(mtcars)

# Select
select(mtcars,mpg,cyl,disp) %>% as_tibble()

#Helper function with select
select(.data = mtcars,starts_with('di')) %>% as_tibble()


# Logical Operators
# <
# <=
# >
# >=
# != not equal to
# | or
# & and
# == Equal to

filter(.data = mtcars,mpg>20,vs==0) %>% as_tibble()

head(iris)

filter(.data = iris,Species%in%c('setosa','versicolor'))

# Mutate
mutate(.data = iris,Weight=Sepal.Length*10) %>% as_tibble()

mutate(.data = mtcars,Height=hp/drat) %>%
  select(-hp) %>% as_tibble()

# Transmute
transmute(.data = iris,
Weight=Sepal.Length*10) %>% as_tibble()


iris %>% group_by(Species) %>%
  summarise(n=n(),Mean=mean(Sepal.Length),SD=sd(Sepal.Length)) %>%
  arrange(desc(SD))


# Forcat

# fct_recode
# fct_colapse
# fct_reorder and fct_reorder2

data(gss_cat)
head(gss_cat)

levels(gss_cat$partyid)


# fct_recode
gss_cat %>%
  mutate(partyid=fct_recode(partyid,'Not strong republican'='Not str republican','Independent near republican'='Ind,near rep','Independent near democrat'='Ind,near dem','Not strong democrat'='Not str democrat'))


# fct_colapse
gss_cat %>%
  mutate(partyid=fct_collapse(partyid,Deep=c("No answer","Strong republican","Independent","Strong democrat"),Dip=c("Don't know","Not str republican","Ind,near dem"),Strong=c("Other party","Ind,near rep","Not str democrat")))


# fct_reorder and fct_reorder2

gss_cat %>%
  group_by(relig) %>%
  summarise(n=n(),tvhours=mean(tvhours,na.rm=T)) %>%
  mutate(relig=fct_reorder(.f = relig,tvhours)) %>%
  ggplot(aes(tvhours,relig))+geom_point()

