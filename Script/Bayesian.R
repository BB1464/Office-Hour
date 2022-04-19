
############################################################################
############################################################################
###                                                                      ###
###                   BAYESIAN ANALYSIS IN R WITH BRMS                   ###
###                                                                      ###
############################################################################
############################################################################

library(brms)
library(posterior) # tools for working with posterior and prior distributions
library(R2jags) # run Jags from within R
library(lme4) # fit GLMM in frequentist framework

dat <- data.frame(alive = 19, total = 57)
dat

# Fit the first model with glm

mle <- glm(cbind(alive, total - alive) ~ 1,
           family = binomial("logit"), # family = binomial("identity") would be more straightforward
           data = dat)

# Get the estimate

coef(mle) # logit scale

# After back-transformation using the reciprocal logit, we obtain:

plogis(coef(mle))


# Bayesian Analysis with Jags

# model
betabin <- function(){
  alive ~ dbin(survival, total) # binomial likelihood
  logit(survival) <- beta # logit link
  beta ~ dnorm(0, 1/1.5) # prior
}
# data
datax <- list(total = 57,
              alive = 19)
# initial values
inits <- function() list(beta = rnorm(1, 0, sd = 1.5))
# parameter to monitor
params <- c("survival")
# run jags
bayes.jags <- jags(data = datax,
                   inits = inits,
                   parameters.to.save = params,
                   model.file = betabin,
                   n.chains = 2,
                   n.iter = 5000,
                   n.burnin = 1000,
                   n.thin = 1)



bayes.jags


# BRMS function for Bayesian Analysis in R

bayes.brms <- brm(alive | trials(total) ~ 1,
                  family = binomial("logit"), # binomial("identity") would be more straightforward
                  data = dat,
                  chains = 2, # nb of chains
                  iter = 5000, # nb of iterations, including burnin
                  warmup = 1000, # burnin
                  thin = 1) # thinning


# Get the result

bayes.brms


# Visualize the posterior density and trace of survival (on the logit scale):

plot(bayes.brms)


# To get survival on the $[0,1]$ scale, we extract the MCMC values, then apply the reciprocal logit function to each of these values, and summarize its posterior distribution:

draws_fit <- as_draws_matrix(bayes.brms)
draws_fit

summarize_draws(plogis(draws_fit[,1]))


# What is the prior used by default?

prior_summary(bayes.brms)



# Continuation

# Logistic regression with covariates
# In this example, we ask whether annual variation in white stork breeding success can be explained by rainfall in the wintering area. Breeding success is measured by the ratio of the number of chicks over the number of pairs.


nbchicks <- c(151,105,73,107,113,87,77,108,118,122,112,120,122,89,69,71,53,41,53,31,35,14,18)
nbpairs <- c(173,164,103,113,122,112,98,121,132,136,133,137,145,117,90,80,67,54,58,39,42,23,23)
rain <- c(67,52,88,61,32,36,72,43,92,32,86,28,57,55,66,26,28,96,48,90,86,78,87)
dat <- data.frame(nbchicks = nbchicks,
                  nbpairs = nbpairs,
                  rain = (rain - mean(rain))/sd(rain)) # standardized rainfall

# The data are grouped.

# Maximum-likelihood estimation
# You can get maximum likelihood estimates with:

  mle <- glm(cbind(nbchicks, nbpairs - nbchicks) ~ rain,
             family = binomial("logit"),
             data = dat)
summary(mle)

# There is a negative effect of rainfall on breeding success:

visreg::visreg(mle, scale = "response")



# Bayesian analysis with brms
# With brms, we write:

bayes.brms <- brm(nbchicks | trials(nbpairs) ~ rain,
                    family = binomial("logit"),
                    data = dat,
                    chains = 2, # nb of chains
                    iter = 5000, # nb of iterations, including burnin
                    warmup = 1000, # burnin
                    thin = 1)

# Display results:

bayes.brms


# Visualize:

plot(bayes.brms)

# We can also calculate the posterior probability of the rainfall effect being below zero with the hypothesis() function:

hypothesis(bayes.brms, 'rain < 0')

# These results confirm the important negative effect of rainfall on breeding success.

# Linear mixed model
# In this example, we have several measurements for 33 Mediterranean plant species, specifically number of seeds and biomass. We ask whether there is a linear relationship between these two variables that would hold for all species.



# Read in data, directly from the course website:

df <- readr::read_csv2("https://raw.githubusercontent.com/oliviergimenez/bayesian-stats-with-R/master/slides/dat/VMG.csv")

df


# Use relevant format for columns species Sp and biomass Vm:

df$Sp <- as.factor(df$Sp)
df$Vm <- as.numeric(df$Vm)

# Define numeric vector of species, for nested indexing in Jags:

species <- as.numeric(df$Sp)

# Define response variable, number of seeds:

y <- log(df$NGrTotest)

# Standardize explanatory variable, biomass

x <- (df$Vm - mean(df$Vm))/sd(df$Vm)

# Now build dataset:

dat <- data.frame(y = y, x = x, species = species)

# Maximum-likelihood estimation
# You can get maximum likelihood estimates for a linear regression of number of seeds on biomass with a species random effect on the intercept (partial pooling):

mle <- lmer(y ~ x + (1 | species), data = dat)

summary(mle)


# Bayesian analysis with Jags
# In Jags, you would use:

  # partial pooling model
  partial_pooling <- function(){
    # likelihood
    for(i in 1:n){
      y[i] ~ dnorm(mu[i], tau.y)
      mu[i] <- a[species[i]] + b * x[i]
    }
    for (j in 1:nbspecies){
      a[j] ~ dnorm(mu.a, tau.a)
    }
    # priors
    tau.y <- 1 / (sigma.y * sigma.y)
    sigma.y ~ dunif(0,100)
    mu.a ~ dnorm(0,0.01)
    tau.a <- 1 / (sigma.a * sigma.a)
    sigma.a ~ dunif(0,100)
    b ~ dnorm(0,0.01)
  }

# data
mydata <- list(y = y,
               x = x,
               n = length(y),
               species = species,
               nbspecies = length(levels(df$Sp)))
# initial values
inits <- function() list(mu.a = rnorm(1,0,5),
                         sigma.a = runif(0,0,10),
                         b = rnorm(1,0,5),
                         sigma.y = runif(0,0,10))
# parameters to monitor
params <- c("mu.a", "sigma.a", "b", "sigma.y")


# Bayesian analysis with brms
# In brms, you write:

bayes.brms <- brm(y ~ x + (1 | species),
                    data = dat,
                    chains = 2, # nb of chains
                    iter = 5000, # nb of iterations, including burnin
                    warmup = 1000, # burnin
                    thin = 1)


# Display the results:

bayes.brms


# You can extract a block of fixed effects:

summary(bayes.brms)$fixed

# And a block of random effects:

summary(bayes.brms)$random


# And visualize:

plot(bayes.brms)




# Let’s simulate some data:

set.seed(666)
transects <- 10
data <- NULL
for (tr in 1:transects){
# random effect (intercept)
ref <- rnorm(1,0,.5)
# water temperature gradient
t <- runif(1, 18,22) + runif(1,-.2,0.2)*1:20
# Anemone gradient (expected response)
ans <- exp(ref -14 + 1.8 * t - 0.045 * t^2)
# actual counts on 20 segments of the current transect
an <- rpois(20, ans)
data <- rbind(data, cbind(rep(tr, 20), t, an))
}

data <- data.frame(Transect = data[,1],
Temperature = data[,2],
Anemones = data[,3])
# Standardize temperature:

boo <- data$Temperature
data$Temp <- (boo - mean(boo)) / sd(boo)
head(data)

#Maximum-likelihood estimation
# With lme4, you write:

fit_lme4 <- glmer(Anemones ~ Temp + I(Temp^2) + (1 | Transect),
data = data,
family = poisson)
summary(fit_lme4)


# Bayesian analysis with Jags
# In Jags, we fit the corresponding GLMM with:

model <- function() {
for (i in 1:n){
count[i] ~ dpois(lambda[i])
log(lambda[i]) <- a[transect[i]] + b[1] * x[i] + b[2] * pow(x[i],2)
}
for (j in 1:nbtransects){
a[j] ~ dnorm (mu.a, tau.a)
}
mu.a ~ dnorm (0, 0.01)
tau.a <- pow(sigma.a, -2)
sigma.a ~ dunif (0, 100)
b[1] ~ dnorm (0, 0.01)
b[2] ~ dnorm (0, 0.01)
}
dat <- list(n = nrow(data),
nbtransects = transects,
x = data$Temp,
count = data$Anemones,
transect = data$Transect)
inits <- function() list(a = rnorm(transects),
b = rnorm(2),
mu.a = rnorm(1),
sigma.a = runif(1))
par <- c ("a", "b", "mu.a", "sigma.a")
fit <- jags(data = dat,
inits = inits,
parameters.to.save = par,
model.file = model,
n.chains = 2,
n.iter = 5000,
n.burn = 1000)
## Compiling model graph
## Resolving undeclared variables
## Allocating nodes
## Graph information:
## Observed stochastic nodes: 200
## Unobserved stochastic nodes: 14
## Total graph size: 1622
##
## Initializing model
round(fit$BUGSoutput$summary[, -c(4,6)], 3)



# Bayesian analysis with brms
# What about with brms?

bayes.brms <- brm(Anemones ~ Temp + I(Temp^2) + (1 | Transect),
data = data,
family = poisson("log"),
chains = 2, # nb of chains
iter = 5000, # nb of iterations, including burnin
warmup = 1000, # burnin
thin = 1)

# Display results:

bayes.brms


#Visualize:

plot(bayes.brms)

#We can assess the quality of fit of this model:

pp_check(bayes.brms, ndraws = 100, type = 'ecdf_overlay')

#As expected, the fit is almost perfect because we simulated data from this very model.

# What if we’d like to test the effect of temperature using WAIC?

# We fit a model with no effect of temperature:

bayes.brms2 <- brm(Anemones ~ 1 + (1 | Transect),
data = data,
family = poisson("log"),
chains = 2, # nb of chains
iter = 5000, # nb of iterations, including burnin
warmup = 1000, # burnin
thin = 1)

# Then we compare both models, by ranking them with their WAIC or using ELPD:

(waic1 <- waic(bayes.brms)) # waic model w/ tempterature


(waic2 <- waic(bayes.brms2)) # waic model wo/ tempterature


loo_compare(waic1, waic2)


# Conclusions

# In all examples, you can check that i) there is little or no difference between Jags and brms numeric summaries of posterior distributions, and ii) maximum likelihood parameter estimates (glm(), lmer() and glmer() functions) and posterior means/medians are very close to each other.

# In contrast to Jags, Nimble or Stan, you do not need to code the likelihood yourself in brms, as long as it is available in the package. You can have a look to a list of distributions with ?brms::brmsfamily, and there is also a possibility to define custom distributions with custom_family().

# Note I haven’t covered diagnostics of convergence, more here. You can also integrate brms outputs in a tidy workflow with tidybayes, and use the ggplot() magic.

# As usual, the code is on GitHub, visit https://github.com/oliviergimenez/fit-glmm-with-brms.

# Now I guess it’s up to the students to pick their favorite Bayesian tool.
