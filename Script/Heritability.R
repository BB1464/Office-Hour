
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


