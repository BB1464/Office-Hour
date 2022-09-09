
###########################################################################
###########################################################################
###                                                                     ###
###                    HERITABILITY FROM MIXED MODEL                    ###
###                                                                     ###
###########################################################################
###########################################################################


library(inti)
library(knitr)

dt <- potato

hr <- H2cal(data = dt
            , trait = "stemdw"
            , gen.name = "geno"
            , rep.n = 5
            , fixed.model = "0 + (1|bloque) + geno"
            , random.model = "1 + (1|bloque) + (1|geno)"
            , emmeans = TRUE
            , plot_diag = TRUE
            , outliers.rm = TRUE
)



hr$model %>% summary()
hr$tabsmr %>% kable(caption = "Variance component table")
hr$blues %>% kable(caption = "BLUEs")
hr$blups %>% kable(caption = "BLUPs")
hr$outliers$fixed %>% kable(caption = "Outliers fixed model")
hr$outliers$random %>% kable(caption = "Outliers random model")


# Extension of the models for all the trait

library(tidyverse)

dt <- potato

# Nested data frame
dat <- dt |>
  pivot_longer(cols = c(4:17)) |>
  group_by(name) |>
  nest()




mod <- function(x){
  hr <- H2cal(data = x
              , trait = "value"
              , gen.name = "geno"
              , rep.n = 5
              , fixed.model = "0 + (1|bloque) + geno"
              , random.model = "1 + (1|bloque) + (1|geno)"
              , emmeans = TRUE
              , plot_diag = TRUE
              , outliers.rm = TRUE
  )
  return(hr)
}



# Fit the model
models <- dat |> mutate(model=map(.x = data,.f = mod )) |>
  select(-data)

# Get the output
models$model |> set_names(models$name)




