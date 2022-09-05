Faith<-read.csv(here::here('Data/Detached Leaf Assay.csv'))


library(tidyverse)
library(agricolae)


Res <- Faith |> pivot_longer(cols = c(6:9)) |>
mutate(across(.cols = c(1:6),.fns = factor)) |>
group_by(name) |> nest() |>
mutate(model=map(.x = data,.f = ~lm(formula = value~Replicates+Age.of.leaves.in.weeks*Trials*Cowpea.Genotype*Isolate.Code,data = .x))) |>
mutate(Anova=map(.x = model,.f = ~anova(.x))) |>
mutate(Anova_tidy=map(.x = Anova,.f = broom::tidy)) |>
mutate(Age_Week=map(.x = model,.f = ~HSD.test(y = .x,trt = 'Age.of.leaves.in.weeks',console = TRUE))) |>
mutate(Trials=map(.x = model,.f = ~HSD.test(y = .x,trt = 'Trials',console = TRUE))) |>
mutate(Genotypes=map(.x = model,.f = ~HSD.test(y = .x,trt = 'Cowpea.Genotype',console = TRUE))) |>
mutate(Isolate=map(.x = model,.f = ~HSD.test(y = .x,trt = 'Isolate.Code',console = TRUE)))






# Get the Anova Table for All the models
Res |>
  pluck('Anova') |>
  set_names(Res$name)


# Get the Mean comparison for Week

Res |>
  pluck('Age_Week') |>
  set_names(Res$name)


# Get the Means Comparison for Trials

Res |>
  pluck('Trials') |>
  set_names(Res$name)

# Get the Post Hoc for Genotypes
Res |>
  pluck('Genotypes') |>
  set_names(Res$name)

# Get the Post Hoc for Isolates

Res |>
  pluck('Isolate') |>
  set_names(Res$name)






