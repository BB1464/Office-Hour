
library(metan)
library(tidyverse)



# Import the dataset ------------------------------------------------------

dat <- read_csv(here::here('Data/GGE Biplot Gbemi.csv'))

# Convert Character to Factor ---------------------------------------------

dat <- dat |>
  mutate(across(.cols = c(1:4),.fns = factor))

# Check the dataset

inspect(dat)


## Summary of the dataset
ge_details(dat,
           env = ENV,
           gen = GEN,
           resp = YLD)

## Heat Map
ge_plot(dat, GEN, ENV, YLD)


mge <- ge_means(dat,
                env = ENV,
                gen = GEN,
                resp = YLD)



# Genotype-environment means

get_model_data(mge) %>% round_cols() |>
  data.frame()


# Environment means

get_model_data(mge, what = "env_means") %>% round_cols() |>
  data.frame()

# Genotype Means
get_model_data(mge, what = "gen_means") %>% round_cols() |>
  data.frame()


## GGE Model

gge_model <- gge(dat, ENV, GEN, YLD)

# Visualize the model output

## GGE biplot
plot(gge_model,type = 1)

## Save the Plot
ggsave('GGE_biplot.png',path = here::here('Plot'),width = 5,height = 5,dpi = 400,bg = 'white')


## Mean vs Stability
plot(gge_model,type = 2)

## Save the Plot
ggsave('Stability.png',path = here::here('Plot'),width = 5,height = 5,dpi = 400,bg = 'white')


## Which-won-Where
plot(gge_model,type = 3)

## Save the Plot
ggsave('which_won_where.png',path = here::here('Plot'),width = 5,height = 5,dpi = 400,bg = 'white')

## Discriminativeness vs representativeness
plot(gge_model,type = 4)

## Save the Plot
ggsave('discriminativeness.png',path = here::here('Plot'),width = 5,height = 5,dpi = 400,bg = 'white')


## Ranking Environment
plot(gge_model,type = 6)

## Save the Plot
ggsave('ranking_environment.png',path = here::here('Plot'),width = 5,height = 5,dpi = 400,bg = 'white')


## Ranking Genotype
plot(gge_model,type = 8)

## Save the Plot
ggsave('ranking_genotype.png',path = here::here('Plot'),width = 5,height = 5,dpi = 400,bg = 'white')


## Compute all the stability statistics


#Annicchiarico(.data = dat,env = ENV,gen = GEN,rep = REP,resp = YLD)


## Get the Winning Genotype

ge_winners(.data = dat,env = ENV,gen = GEN,resp = YLD,type = 'winners')



ge_winners(.data = dat,env = ENV,gen = GEN,resp = YLD,type = 'ranks') |>
  data.frame()
