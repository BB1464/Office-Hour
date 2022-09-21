library(inti)

dt <- potato
hr <- H2cal(data = dt
            , trait = "stemdw"
            , gen.name = "geno"
            , rep.n = 5
            , fixed.model = "0 + (1|bloque) + geno"
            , random.model = "1 + (1|bloque) + (1|geno)"
            , emmeans = TRUE
            , plot_diag = TRUE
            , outliers.rm = TRUE,summary = TRUE)

hr$tabsmr
hr$blues
hr$blups
hr$outliers
hr$diagplot
