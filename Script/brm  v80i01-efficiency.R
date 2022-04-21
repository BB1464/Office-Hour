## loading required packages
library("MCMCglmm")  # version 2.24
library("rethinking")  # version 1.59
library("rstanarm")  # version 2.15.3
library("brms")  # version 1.8.0

## helper function to better compute the effective sample size
eff_size <- function(x) {
  if (is(x, "brmsfit")) {
    samples <- as.data.frame(x$fit)
  } else if (is(x, "stanreg") || is(x, "map2stan")) {
    samples <- as.data.frame(x$stanfit)
  } else if (is(x, "stanfit")) {
    samples <- as.data.frame(x)
  } else if (is(x, "MCMCglmm")) {
    samples <- cbind(x$Sol, x$VCV)
  } else {
    stop("invalid input")
  }
  # call an internal function of rstan
  floor(apply(samples, MARGIN = 2, FUN = rstan:::ess_rfun))
}

## ----- compare efficiency between packages -----
# only used for Stan packages
iter <- 6000
warmup <- 1000
chains <- 1
adapt_delta <- 0.8
# only used for MCMCglmm
nitt <- 35000
burnin <- 10000
thin <- 5
# leads to 5000 posterior samples


## --------- Dyestuff ---------
# brms
prior_dye_brms <- c(set_prior("normal(0, 2000)", class = "Intercept"),
                    set_prior("cauchy(0, 50)", class = "sd"),
                    set_prior("cauchy(0, 50)", class = "sigma"))
dye_brms <- brm(Yield ~ 1 + (1 | Batch), data = lme4::Dyestuff,
                prior = prior_dye_brms, chains = 0)
time_dye_brms <- system.time(capture.output(
  dye_brms <- update(dye_brms, iter = iter, warmup = warmup, chains = chains,
                     control = list(adapt_delta = adapt_delta))
))
# summary(dye_brms)
eff_dye_brms <- min(eff_size(dye_brms)) / time_dye_brms[[1]]

# rstanarm
time_dye_rstanarm <- system.time(capture.output(
  dye_rstanarm <- stan_glmer(Yield ~ 1 + (1 | Batch), data = lme4::Dyestuff,
                             prior_intercept = normal(0, 2000),
                             iter = iter, warmup = warmup, chains = chains,
                             adapt_delta = adapt_delta)
))
# summary(dye_rstanarm)
eff_dye_rstanarm <- min(eff_size(dye_rstanarm)) / time_dye_rstanarm[[1]]

# rethinking
dye_flist <- alist(
  Yield ~ dnorm(eta, sigma),
  eta <- a + a_Batch[Batch],
  a ~ dnorm(0,2000),
  a_Batch[Batch] ~ dnorm(0, sd_Batch),
  sigma ~ dcauchy(0, 50),
  sd_Batch ~ dcauchy(0, 50))
# cannot set chains = 0
dye_rethinking <- map2stan(dye_flist, data = lme4::Dyestuff, iter = 1)
time_dye_rethinking <- system.time(capture.output(
  dye_rethinking <- stan(fit = dye_rethinking@stanfit, data = dye_rethinking@data,
                         iter = iter, warmup = warmup, chains = chains,
                         control = list(adapt_delta = adapt_delta))
))
# summary(dye_rethinking)
eff_dye_rethinking <- min(eff_size(dye_rethinking)) / time_dye_rethinking[[1]]

# MCMCglmm
time_dye_MCMCglmm <- system.time(capture.output(
  dye_MCMCglmm <- MCMCglmm(Yield ~ 1,
                           random = ~ Batch, data = lme4::Dyestuff,
                           thin = thin, nitt = nitt, burnin = burnin)
))
# summary(dye_MCMCglmm)
eff_dye_MCMCglmm <- min(eff_size(dye_MCMCglmm)) / time_dye_MCMCglmm[[1]]

# print efficiency
print(c(brms = eff_dye_brms, rstanarm = eff_dye_rstanarm,
        rethinking = eff_dye_rethinking, MCMCglmm = eff_dye_MCMCglmm))


## ---------- lme4::cake ----------
# brms
prior_cake_brms <- c(set_prior("normal(0,10)", class = "b"),
                     set_prior("normal(0,50)", class = "Intercept"),
                     set_prior("cauchy(0, 10)", class = "sd"),
                     set_prior("cauchy(0, 10)", class = "sigma"))
cake_brms <- brm(angle ~ recipe * temperature + (1 | recipe : replicate),
                 data = lme4::cake, prior = prior_cake_brms, chains = 0)
time_cake_brms <- system.time(capture.output(
  cake_brms <- update(cake_brms, iter = iter,
                      warmup = warmup, chains = chains,
                      control = list(adapt_delta = adapt_delta))
))
# summary(cake_brms)
eff_cake_brms <- min(eff_size(cake_brms)) / time_cake_brms[[1]]

# rstanarm
time_cake_rstanarm <- system.time(capture.output(
  cake_rstanarm <- stan_glmer(angle ~ recipe * temperature + (1 | recipe : replicate),
                              data = lme4::cake, prior = normal(0,10),
                              prior_intercept = normal(0,50),
                              iter = iter, warmup = warmup, chains = chains,
                              adapt_delta = adapt_delta)
))
# summary(cake_rstanarm)
eff_cake_rstanarm <- min(eff_size(cake_rstanarm)) / time_cake_rstanarm[[1]]

# rethinking
cake2 <- as.data.frame(model.matrix(angle ~ recipe * temperature,
                                 data = lme4::cake)[, -1])
colnames(cake2) <- paste0("x", 1:17)
cake2$group <- factor(with(lme4::cake, paste0(recipe, "_", replicate)))
cake2$angle <- lme4::cake$angle
cake_flist <- alist(
  angle ~ dnorm(eta, sigma),
  eta <- a + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6 + b7*x7 + b8*x8 + b9*x9
         + b10*x10 + b11*x11 + b12*x12 + b13*x13 + b14*x14 + b15*x15+ b16*x16
         + b17*x17 + a_group[group],
  a ~ dnorm(0, 50),
  c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17) ~ dnorm(0, 10),
  a_group[group] ~ dnorm(0, sd_group),
  sigma ~ dcauchy(0, 10),
  sd_group ~ dcauchy(0, 10))
cake_rethinking <- map2stan(cake_flist, data = cake2, iter = 1)
time_cake_rethinking <- system.time(capture.output(
  cake_rethinking <- stan(fit = cake_rethinking@stanfit, data = cake_rethinking@data,
                         iter = iter, warmup = warmup, chains = chains,
                         control = list(adapt_delta = adapt_delta))
))
# summary(cake_rethinking)
eff_cake_rethinking <- min(eff_size(cake_rethinking)) / time_cake_rethinking[[1]]

# MCMCglmm
prior_cake_MCMCglmm <- list(B = list(mu = rep(0, 18),
                                     V = diag(c(50^2, rep(100, 17)))),
                            R = list(V = 1, nu = 0.002),
                            G = list(G1 = list(V = 1, nu = 0.002)))
time_cake_MCMCglmm <- system.time(capture.output(
  cake_MCMCglmm <- MCMCglmm(angle ~ recipe * temperature,
                            random = ~ recipe:replicate, data = lme4::cake,
                            prior = prior_cake_MCMCglmm,
                            thin = thin, nitt = nitt, burnin = burnin)
))
# summary(cake_MCMCglmm)
eff_cake_MCMCglmm <- min(eff_size(cake_MCMCglmm)) / time_cake_MCMCglmm[[1]]

# print efficiency
print(c(brms = eff_cake_brms, rstanarm = eff_cake_rstanarm,
        rethinking = eff_cake_rethinking, MCMCglmm = eff_cake_MCMCglmm))


## ---------- lme4::sleepstudy ----------
# brms
prior_sleep_brms <- c(set_prior("normal(0,25)", class = "b"),
                      set_prior("normal(0,500)", class = "Intercept"),
                      set_prior("cauchy(0,30)", class = "sd"),
                      set_prior("cauchy(0,30)", class = "sigma"))
sleep_brms <- brm(Reaction ~ Days + (Days | Subject),
                  data = lme4::sleepstudy,
                  prior = prior_sleep_brms, chains = 0)
time_sleep_brms <- system.time(capture.output(
  sleep_brms <- update(sleep_brms, iter = iter,
                       warmup = warmup, chains = chains,
                       control = list(adapt_delta = adapt_delta))
))
# summary(sleep_brms)
eff_sleep_brms <- min(eff_size(sleep_brms)) / time_sleep_brms[[1]]

# rstanarm
time_sleep_rstanarm <- system.time(capture.output(
  sleep_rstanarm <- stan_glmer(Reaction ~ Days + (Days | Subject),
                               lme4::sleepstudy, adapt_delta = adapt_delta,
                               prior = normal(0,25), prior_intercept = normal(0,500),
                               iter = iter, warmup = warmup, chains = chains)
))
# summary(sleep_rstanarm)
eff_sleep_rstanarm <- min(eff_size(sleep_rstanarm)) / time_sleep_rstanarm[[1]]

# rethinking
sleep_flist <- alist(
  Reaction ~ dnorm(eta, sigma),
  eta <- A + B * Days,
  A <- a + a_subject[Subject],
  B <- b + b_subject[Subject],
  a ~ dnorm(0, 500),
  b ~ dnorm(0, 25),
  c(a_subject,b_subject)[Subject] ~ dmvnormNC(sd_subject, Rho_subject),
  sigma ~ dcauchy(0, 30),
  sd_subject ~ dcauchy(0, 30),
  Rho_subject ~ dlkjcorr(1))
sleep_rethinking <- suppressWarnings(
  map2stan(sleep_flist, data = lme4::sleepstudy, iter = 1))
time_sleep_rethinking <- system.time(capture.output(
  sleep_rethinking <- stan(fit = sleep_rethinking@stanfit,
                           data = sleep_rethinking@data,
                           pars = c("eta", "B", "A"), include = FALSE,
                           iter = iter, warmup = warmup, chains = chains,
                           control = list(adapt_delta = adapt_delta))
))
# summary(sleep_rethinking)
eff_sleep_rethinking <- min(eff_size(sleep_rethinking)) / time_sleep_rethinking[[1]]

# MCMCglmm
# priors are chosen in an attempt to get the model running
prior_sleep_MCMCglmm <- list(R = list(V = 1, nu = 1),
                             G = list(G1 = list(V = diag(2), nu = 2)))
time_sleep_MCMCglmm <- system.time(capture.output(
  sleep_MCMCglmm <- MCMCglmm(Reaction ~ Days, random = ~ us(1 + Days):units,
                            data = lme4::sleepstudy,
                            prior = prior_sleep_MCMCglmm,
                            thin = thin, nitt = nitt, burnin = burnin)
))
# summary(sleep_MCMCglmm)
eff_sleep_MCMCglmm <- min(eff_size(sleep_MCMCglmm)) / time_sleep_MCMCglmm[[1]]

# print efficiency
print(c(brms = eff_sleep_brms, rstanarm = eff_sleep_rstanarm,
        rethinking = eff_sleep_rethinking, MCMCglmm = eff_sleep_MCMCglmm))


## ---------- lme4::cbpp ----------
cbpp <- lme4::cbpp
cbpp$obs <- 1:nrow(cbpp)
# brms
prior_cbpp_brms <- c(set_prior("normal(0,5)", class = "b"),
                     set_prior("normal(0,5)", class = "Intercept"),
                     set_prior("cauchy(0,5)", class = "sd"))
cbpp_brms <- brm(incidence | trials(size) ~ period + (1 | herd) + (1 | obs),
                 family = binomial(), data = cbpp,
                 prior = prior_cbpp_brms, chains = 0)
time_cbpp_brms <- system.time(capture.output(
  cbpp_brms <- update(cbpp_brms, iter = iter,
                      warmup = warmup, chains = chains,
                      control = list(adapt_delta = adapt_delta))
))
# summary(cbpp_brms)
eff_cbpp_brms <- min(eff_size(cbpp_brms)) / time_cbpp_brms[[1]]

# rstanarm
time_cbpp_rstanarm <- system.time(capture.output(
  cbpp_rstanarm <- stan_glmer(cbind(incidence, size - incidence) ~
                                period + (1 | herd) + (1 | obs),
                              family = "binomial", data = cbpp,
                              prior = normal(0,5), prior_intercept = normal(0,5),
                              iter = iter, warmup = warmup, chains = chains,
                              adapt_delta = adapt_delta)
))
# summary(cbpp_rstanarm)
eff_cbpp_rstanarm <- min(eff_size(cbpp_rstanarm)) / time_cbpp_rstanarm[[1]]

# rethinking
cbpp2 <- as.data.frame(model.matrix(~ period, data = cbpp)[, -1])
names(cbpp2) <- paste0("x", 1:3)
cbpp2 <- cbind(cbpp2, cbpp)
# size is a reseverd word in Stan
names(cbpp2)[names(cbpp2) == "size"] <- "size2"
cbpp_flist <- alist(
  incidence ~ dbinom(size2, eta),
  logit(eta) <- a + b1*x1 + b2*x2 + b3*x3 + a_herd[herd] + a_obs[obs],
  a ~ dnorm(0,5),
  c(b1,b2,b3) ~ dnorm(0,5),
  a_herd[herd] ~ dnorm(0, sd_herd),
  a_obs[obs] ~ dnorm(0, sd_obs),
  sd_herd ~ dcauchy(0, 5),
  sd_obs ~ dcauchy(0, 5))
# I couldn't find a way to tell map2stan that size2 should be an array of integers...
# cbpp_rethinking <- map2stan(cbpp_flist, data = cbpp2, iter = 1)
# time_cbpp_rethinking <- system.time(capture.output(
#   cbpp_rethinking <- stan(fit = cbpp_rethinking@stanfit, data = cbpp_rethinking@data,
#                          iter = iter, warmup = warmup, chains = chains,
#                          control = list(adapt_delta = adapt_delta))
# ))
# summary(cbpp_rethinking)
# eff_cbpp_rethinking <- min(eff_size(cbpp_rethinking)) / time_cbpp_rethinking[[1]]

# MCMCglmm
time_cbpp_MCMCglmm <- system.time(capture.output(
  cbpp_MCMCglmm <- MCMCglmm(cbind(incidence, size - incidence) ~ period,
                            random = ~ herd, data = cbpp,
                            prior = list(R = list(V = 1, nu = 0.002),
                                         G = list(G1 = list(V = 1, nu = 0.002))),
                            family = "multinomial2",
                            nitt = nitt, thin = thin, burnin = burnin)
))
# summary(cbpp_MCMCglmm)
eff_cbpp_MCMCglmm <- min(eff_size(cbpp_MCMCglmm)) / time_cbpp_MCMCglmm[[1]]

# print efficiency
print(c(brms = eff_cbpp_brms, rstanarm = eff_cbpp_rstanarm,
        MCMCglmm = eff_cbpp_MCMCglmm))


## ---------- lme4::grouseticks ----------
# brms
prior_grouse_brms <- c(set_prior("normal(0,5)", class = "b"),
                     set_prior("normal(0,20)", class = "Intercept"),
                     set_prior("cauchy(0,5)", class = "sd"))
grouse_brms <- brm(TICKS ~ YEAR + HEIGHT + (1 | BROOD) + (1 | LOCATION),
                   family = poisson(), data = lme4::grouseticks,
                   prior = prior_grouse_brms, chains = 0)
time_grouse_brms <- system.time(capture.output(
  grouse_brms <- update(grouse_brms, iter = iter,
                      warmup = warmup, chains = chains,
                      control = list(adapt_delta = 0.95))
))
# summary(grouse_brms)
eff_grouse_brms <- min(eff_size(grouse_brms)) / time_grouse_brms[[1]]

# rstanarm
time_grouse_rstanarm <- system.time(capture.output(
  grouse_rstanarm <- stan_glmer(TICKS ~ YEAR + HEIGHT + (1 | BROOD) + (1 | LOCATION),
                                family = poisson(), data = lme4::grouseticks,
                                prior = normal(0,5), prior_intercept = normal(0,20),
                                iter = iter, warmup = warmup, chains = chains,
                                adapt_delta = 0.95)
))
# summary(grouse_rstanarm)
eff_grouse_rstanarm <- min(eff_size(grouse_rstanarm)) / time_grouse_rstanarm[[1]]

# rethinking
grouseticks2 <- as.data.frame(model.matrix(~ YEAR, data = lme4::grouseticks)[, -1])
names(grouseticks2) <- c("YEAR96", "YEAR97")
grouseticks2 <- cbind(grouseticks2,
                      lme4::grouseticks[, c("TICKS", "HEIGHT", "BROOD", "LOCATION")])
grouse_flist <- alist(
  TICKS ~ dpois(eta),
  log(eta) <- a + b1*YEAR96 + b2*YEAR97 + b3*HEIGHT + a_brood[BROOD] + a_location[LOCATION],
  a ~ dnorm(0,20),
  c(b1,b2,b3) ~ dnorm(0,5),
  a_brood[BROOD] ~ dnorm(0, sd_brood),
  a_location[LOCATION] ~ dnorm(0, sd_location),
  sd_brood ~ dcauchy(0, 5),
  sd_location ~ dcauchy(0, 5))
grouse_rethinking <- suppressWarnings(
  map2stan(grouse_flist, data = grouseticks2, iter = 1))
# increasing adapt_delta beyond 0.95 may be required here
time_grouse_rethinking <- system.time(capture.output(
  grouse_rethinking <- stan(fit = grouse_rethinking@stanfit,
                            data = grouse_rethinking@data,
                            pars = "eta", include = FALSE,
                            iter = iter, warmup = warmup, chains = chains,
                            control = list(adapt_delta = 0.95))
))
# summary(grouse_rethinking)
eff_grouse_rethinking <- min(eff_size(grouse_rethinking)) / time_grouse_rethinking[[1]]

# MCMCglmm always assumes residual terms, which is not feasible for this example

# print efficiency
print(c(brms = eff_grouse_brms, rstanarm = eff_grouse_rstanarm,
        rethinking = eff_grouse_rethinking))


## ---------- lme4::VerbAgg ----------
# brms
prior_verbagg_brms <- c(set_prior("normal(0,5)", class = "b"),
                        set_prior("normal(0,10)", class = "Intercept"),
                        set_prior("cauchy(0,5)", class = "sd"))
verbagg_brms <- brm(r2 ~ (Anger + Gender + btype + situ)^2 + (1 | id) + (1 | item),
                    family = bernoulli(), data = lme4::VerbAgg,
                    prior = prior_verbagg_brms, chains = 0)
time_verbagg_brms <- system.time(capture.output(
  verbagg_brms <- update(verbagg_brms, iter = iter,
                         warmup = warmup, chains = chains,
                         control = list(adapt_delta = adapt_delta))
))
# summary(verbagg_brms)
eff_verbagg_brms <- min(eff_size(verbagg_brms)) / time_verbagg_brms[[1]]

# rstanarm
time_verbagg_rstanarm <- system.time(capture.output(
  verbagg_rstanarm <-  stan_glmer(r2 ~ (Anger + Gender + btype + situ)^2 + (1 | id) + (1 | item),
                                  family = binomial(), data = lme4::VerbAgg,
                                  prior = normal(0,5), prior_intercept = normal(0,10),
                                  iter = iter, warmup = warmup, chains = chains,
                                  adapt_delta = adapt_delta)
))
# summary(verbagg_rstanarm)
eff_verbagg_rstanarm <- min(eff_size(verbagg_rstanarm)) / time_verbagg_rstanarm[[1]]

# rethinking
VerbAgg2 <- as.data.frame(model.matrix(~(Anger + Gender + btype + situ)^2,
                                       data = lme4::VerbAgg)[, -1])
names(VerbAgg2) <- paste0("x", 1:14)
VerbAgg2 <- cbind(VerbAgg2, lme4::VerbAgg[, c("r2", "item", "id")])
VerbAgg2$r2 <- ifelse(VerbAgg2$r2 == "Y", 1, 0)
verbagg_flist <- alist(
  r2 ~ dbinom(1, eta),
  logit(eta) <- a + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6 + b7*x7 + b8*x8 + b9*x9
  + b10*x10 + b11*x11 + b12*x12 + b13*x13 + b14*x14 + a_id[id] + a_item[item],
  a ~ dnorm(0, 10),
  c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14) ~ dnorm(0, 5),
  a_id[id] ~ dnorm(0, sd_id),
  sd_id ~ dcauchy(0, 5),
  a_item[item] ~ dnorm(0, sd_item),
  sd_item ~ dcauchy(0, 5))
verbagg_rethinking <- map2stan(verbagg_flist, data = VerbAgg2, iter = 2)
time_verbagg_rethinking <- system.time(capture.output(
  verbagg_rethinking <- stan(fit = verbagg_rethinking@stanfit,
                             data = verbagg_rethinking@data,
                             iter = iter, warmup = warmup, chains = chains,
                             pars = "eta", include = FALSE,
                             control = list(adapt_delta = adapt_delta))
))
# summary(verbagg_rethinking)
eff_verbagg_rethinking <- min(eff_size(verbagg_rethinking)) / time_verbagg_rethinking[[1]]

# print efficiency
print(c(brms = eff_verbagg_brms, rstanarm = eff_verbagg_rstanarm,
        rethinking = eff_verbagg_rethinking))


## ---------- brms::epilepsy ----------
# brms
prior_epi_brms <- c(set_prior("normal(0,5)", class = "b"),
                    set_prior("normal(0,10)", class = "Intercept"),
                    set_prior("cauchy(0,5)", class = "sd"))
epi_brms <- brm(count ~ log_Age_c + log_Base4_c * Trt_c
                + (1 | patient) + (1 | visit) + (1 | obs),
                family = poisson(), data = brms::epilepsy,
                prior = prior_epi_brms, chains = 0)
time_epi_brms <- system.time(capture.output(
  epi_brms <- update(epi_brms, iter = iter,
                     warmup = warmup, chains = chains,
                     control = list(adapt_delta = adapt_delta))
))
# summary(epi_brms)
eff_epi_brms <- min(eff_size(epi_brms)) / time_epi_brms[[1]]

# rstanarm
time_epi_rstanarm <- system.time(capture.output(
  epi_rstanarm <-  stan_glmer(count ~ log_Age_c + log_Base4_c * Trt_c
                              + (1 | patient) + (1 | visit) + (1 | obs),
                              family = poisson(), data = brms::epilepsy,
                              prior = normal(0,5), prior_intercept = normal(0,10),
                              iter = iter, warmup = warmup, chains = chains,
                              adapt_delta = adapt_delta)
))
# summary(epi_rstanarm)
eff_epi_rstanarm <- min(eff_size(epi_rstanarm)) / time_epi_rstanarm[[1]]

# rethinking
epi_flist <- alist(
  count ~ dpois(eta),
  log(eta) <- a + b1 * log_Age_c + b2*log_Base4_c + b3*Trt_c  +
    b4*log_Base4_c*Trt_c + a_visit[visit] + a_patient[patient] + a_obs[obs],
  a ~ dnorm(0,5),
  c(b1,b2,b3,b4) ~ dnorm(0,10),
  a_visit[visit] ~ dnorm(0, sd_visit),
  a_patient[patient] ~ dnorm(0, sd_patient),
  a_obs[obs] ~ dnorm(0, sd_obs),
  sd_visit ~ dcauchy(0, 5),
  sd_patient ~ dcauchy(0, 5),
  sd_obs ~ dcauchy(0, 5))
epi_rethinking <- suppressWarnings(
  map2stan(epi_flist, data = brms::epilepsy, iter = 1))
time_epi_rethinking <- system.time(capture.output(
  epi_rethinking <- stan(fit = epi_rethinking@stanfit,
                         data = epi_rethinking@data,
                         pars = "eta", include = FALSE,
                         iter = iter, warmup = warmup, chains = chains,
                         control = list(adapt_delta = adapt_delta))
))
# summary(epi_rethinking)
eff_epi_rethinking <- min(eff_size(epi_rethinking)) / time_epi_rethinking[[1]]

# MCMCglmm
time_epi_MCMCglmm <- system.time(capture.output(
  epi_MCMCglmm <- MCMCglmm(count ~ log_Age_c + log_Base4_c * Trt_c,
                           random = ~ visit + patient, data = brms::epilepsy,
                           family = "poisson",
                           nitt = nitt, thin = thin, burnin = burnin)
))
# summary(epi_MCMCglmm)
eff_epi_MCMCglmm <- min(eff_size(epi_MCMCglmm)) / time_epi_MCMCglmm[[1]]

# print efficiency
print(c(brms = eff_epi_brms, rstanarm = eff_epi_rstanarm,
        rethinking = eff_epi_rethinking, MCMCglmm = eff_epi_MCMCglmm))


# ---------- brms::kidney ----------
# brms
prior_kidney_brms <- c(set_prior("normal(0,5)", class = "b"),
                       set_prior("normal(0,10)", class = "Intercept"))
kidney_brms <- brm(time ~ age + sex + disease + (1 | patient),
                   data = brms::kidney, family = Gamma("log"),
                   prior = prior_kidney_brms, chains = 0)
time_kidney_brms <- system.time(capture.output(
  kidney_brms <- update(kidney_brms, iter = iter,
                     warmup = warmup, chains = chains,
                     control = list(adapt_delta = adapt_delta))
))
# summary(kidney_brms)
eff_kidney_brms <- min(eff_size(kidney_brms)) / time_kidney_brms[[1]]

# rstanarm
time_kidney_rstanarm <- system.time(capture.output(
  kidney_rstanarm <-  stan_glmer(time ~ age + sex + disease + (1 | patient),
                              data = brms::kidney, family = Gamma("log"),
                              prior = normal(0,5), prior_intercept = normal(0,10),
                              iter = iter, warmup = warmup, chains = chains,
                              adapt_delta = adapt_delta)
))
# summary(kidney_rstanarm)
eff_kidney_rstanarm <- min(eff_size(kidney_rstanarm)) / time_kidney_rstanarm[[1]]

# rethinking
kidney2 <- as.data.frame(model.matrix(~ age + sex + disease,
                                      data = brms::kidney)[, -1])
names(kidney2) <- paste0("x", 1:ncol(kidney2))
kidney2 <- cbind(kidney2, brms::kidney[, c("time", "patient")])
kidney_flist <- alist(
  time ~ dgamma(shape, eta * shape),
  log(eta) <- -(a + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + a_patient[patient]),
  a ~ dnorm(0,5),
  c(b1,b2,b3,b4,b5) ~ dnorm(0,10),
  a_patient[patient] ~ dnorm(0, sd_patient),
  sd_patient ~ dcauchy(0, 5),
  shape ~ dgamma(0.01,0.01))
kidney_rethinking <- suppressWarnings(
  map2stan(kidney_flist, data = kidney2, iter = 1))
time_kidney_rethinking <- system.time(capture.output(
  kidney_rethinking <- stan(fit = kidney_rethinking@stanfit,
                            data = kidney_rethinking@data,
                            pars = "eta", include = FALSE,
                            iter = iter, warmup = warmup, chains = chains,
                            control = list(adapt_delta = adapt_delta))
))
# summary(kidney_rethinking)
eff_kidney_rethinking <- min(eff_size(kidney_rethinking)) / time_kidney_rethinking[[1]]

# MCMCglmm doesn't support Gamma models

# print efficiency
print(c(brms = eff_kidney_brms, rstanarm = eff_kidney_rstanarm,
        rethinking = eff_kidney_rethinking))


# ---------- stata example: alc_use ----------
alcuse <- haven::read_dta("https://stats.idre.ucla.edu/stat/stata/examples/alda/data/alcohol1_pp.dta")
alcuse <- as.data.frame(alcuse)

# brms
prior_alcuse_brms <- c(set_prior("normal(0,50)", class = "b"),
                       set_prior("normal(0,50)", class = "Intercept"),
                       set_prior("cauchy(0,5)", class = "sd"),
                       set_prior("cauchy(0,5)", class = "sigma"))
alcuse_brms <- brm(alcuse ~ age_14 + (age_14 | id), data = alcuse,
                   prior = prior_alcuse_brms, chains = 0)
time_alcuse_brms <- system.time(capture.output(
  alcuse_brms <- update(alcuse_brms, iter = iter,
                        warmup = warmup, chains = chains,
                        control = list(adapt_delta = adapt_delta))
))
# summary(alcuse_brms)
eff_alcuse_brms <- min(eff_size(alcuse_brms)) / time_alcuse_brms[[1]]

# rstanarm
time_alcuse_rstanarm <- system.time(capture.output(
  alcuse_rstanarm <-  stan_glmer(alcuse ~ age_14 + (age_14 | id), data = alcuse,
                                prior = normal(0,50), prior_intercept = normal(0,50),
                                iter = iter, warmup = warmup, chains = chains,
                                adapt_delta = adapt_delta)
))
# summary(alcuse_rstanarm)
eff_alcuse_rstanarm <- min(eff_size(alcuse_rstanarm)) / time_alcuse_rstanarm[[1]]

# rethinking
alcuse_flist <- alist(
  alcuse ~ dnorm(eta, sigma),
  eta <- A + B * age_14,
  A <- a + a_id[id],
  B <- b + b_id[id],
  a ~ dnorm(0, 50),
  b ~ dnorm(0, 50),
  c(a_id, b_id)[id] ~ dmvnormNC(sd_id, Rho_id),
  sigma ~ dcauchy(0, 5),
  sd_id ~ dcauchy(0, 5),
  Rho_id ~ dlkjcorr(1))
alcuse_rethinking <- suppressWarnings(
  map2stan(alcuse_flist, data = alcuse, iter = 1))
time_alcuse_rethinking <- system.time(capture.output(
  alcuse_rethinking <- stan(fit = alcuse_rethinking@stanfit,
                           data = alcuse_rethinking@data,
                           pars = c("eta", "B", "A"), include = FALSE,
                           iter = iter, warmup = warmup, chains = chains,
                           control = list(adapt_delta = adapt_delta))
))
# summary(alcuse_rethinking)
eff_alcuse_rethinking <- min(eff_size(alcuse_rethinking)) / time_alcuse_rethinking[[1]]

# MCMCglmm
time_alcuse_MCMCglmm <- system.time(capture.output(
  alcuse_MCMCglmm <- MCMCglmm(alcuse ~ age_14, random = ~ us(1 + age_14):units,
                              data = alcuse,
                              prior = list(B = list(mu = rep(0, 2), V = diag(50^2, 2)),
                                           R = list(V = 1, nu = 0.002),
                                           G = list(G1 = list(V = diag(2), nu = 0.002))),
                              nitt = nitt, thin = thin, burnin = burnin)
))
# summary(alcuse_MCMCglmm)
eff_alcuse_MCMCglmm <- min(eff_size(alcuse_MCMCglmm)) / time_alcuse_MCMCglmm[[1]]

# print efficiency
print(c(brms = eff_alcuse_brms, rstanarm = eff_alcuse_rstanarm,
        rethinking = eff_alcuse_rethinking, MCMCglmm = eff_alcuse_MCMCglmm))
