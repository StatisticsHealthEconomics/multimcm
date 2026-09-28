
# test exponential cancer time to event
# stan and plots
#
# https://betanalpha.github.io/assets/case_studies/identifying_mixture_models.html

library(purrr)
library(reshape2)
library(dplyr)
library(rstan)
library(shinystan)
library(dplyr)
library(testthat)
library(ggplot2)
# library(rstanbmcm)
# devtools::load_all()

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)

# 1: cancer (both)
# 2: cured (background only)

# sample size
n <- 1000
# n <- 20

cf <- 0.2
z <- rbinom(n, 1, cf) + 1
n_0 <- sum(z == 1)
n_bg <- sum(z == 2)

# age <- rep(c(10,25,50,100), each = n/4)
age <- rep(50, each = n) # all the same age
age_centred <- c(age[z == 2], age[z == 1]) - mean(age)

beta_bg <- 0.005
baseline_bg <- exp(-8)
rates_bg <- baseline_bg * exp(beta_bg*age[z == 2])

beta_0 <- 0.005
baseline_0 <- exp(-2)
rates_0 <- baseline_0 * exp(beta_0*age[z == 1])
rates_bg0 <- baseline_bg * exp(beta_bg*age[z == 1])

# generate samples
times_bg <- rexp(n_bg, rate = rates_bg)
times_0 <- rexp(n_0, rate = rates_0)
times_bg0 <- rexp(n_0, rate = rates_bg0 + rates_0)

# check
##TODO: why signs apparently wrong way around??
# lm(log(times_0) ~ age[z == 1])
# lm(log(times_bg) ~ age[z == 2])

fixed_cf <- FALSE

if (!fixed_cf) {

  data_beta <-
    list(mu_0 = c(log(baseline_0), beta_0),
         sigma_0 = c(1,1),
         mu_bg = c(log(baseline_bg), beta_bg),
         sigma_bg = c(1,1),
         a_cf = 3,
         b_cf = 12,
         n = n,
         t = c(times_bg, times_bg0),
         d = rep(1, n),     # no censoring
         H = 2,
         X = matrix(c(rep(1, n),
                      age_centred),
                    byrow = FALSE,
                    ncol = 2),
         t_max = 60)

  stan_object <- stanmodels$exp_relative_mix

} else {
  data_fixed <-
    list(mu_0 = c(log(baseline_0), beta_0),
         sigma_0 = c(1,1),
         mu_bg = c(log(baseline_bg), beta_bg),
         sigma_bg = c(1,1),
         curefrac = cf,     # point value
         n = n,
         t = c(times_bg, times_bg0),
         d = rep(1, n),
         H = 2,
         X = matrix(c(rep(1, n),
                      age_centred),
                    byrow = FALSE,
                    ncol = 2),
         t_max = 60)

  stan_object <- stanmodels$exp_cf_fixed
}

# res <-
#   rstan::sampling(
#     object = stan_object,
#     data = data_fixed,
#     warmup = 1000,
#     iter = 10000,
#     thin= 10,
#     control = list(adapt_delta = 0.99,
#                    max_treedepth = 20),
#     chains = 1)

# from file
res <-
  rstan::stan(
    file = here::here("inst", "stan", "exp_relative_mix.stan"),
    data = data_beta,
    warmup = 1000,
    iter = 10000,
    thin= 10,
    control = list(adapt_delta = 0.99,
                   max_treedepth = 20),
    chains = 1)

# rstan::check_divergences(res)


#########
# plots #
#########

fit_stan <- extract(res)

expect_equal(mean(fit_stan$curefrac), cf, tolerance = 0.02)

##TODO:no data => posterior == prior

expect_equal(colMeans(fit_stan$beta_bg), beta_bg, tolerance = 2)
expect_equal(colMeans(fit_stan$beta0), beta_0, tolerance = 2)

quantile(fit_stan$curefrac, c(0.025, 0.5, 0.975))


par(mfrow = c(2,2))
hist(fit_stan$beta0[, 1])
hist(fit_stan$beta0[, 2])
hist(fit_stan$beta_bg[, 1])
hist(fit_stan$beta_bg[, 2])

## survival curves

par(mfrow = c(1,3))
# par(mfrow = c(1,1))

# posteriors
plot(colMeans(fit_stan$S_pred), type = "l", ylim = c(0, 1), lwd = 2.5)
lines(colMeans(fit_stan$S_bg), type = "l", col = "red", lwd = 2.5)
lines(colMeans(fit_stan$S_0), type = "l", col = "blue", lwd = 2.5)

# lines(apply(fit_stan$S0, 2, function(x) quantile(x, probs = 0.025)),
# type = "l", col = "blue")

# kaplan-meier
library(survival)

plot(
  survfit(Surv(times_bg, rep(1, n_bg)) ~ 1),
  col = "red",
  xlim = c(0, 60))
lines(
  survfit(Surv(times_0, rep(1, n_0)) ~ 1),
  col = "blue")
lines(
  survfit(Surv(c(times_bg, times_0), rep(1, n)) ~ 1))

# true values
# for each age in sample
times <- 0:60
plot(NA, type = 'n', xlim = c(0,60), ylim = c(0,1))
lapply(rates_bg,
       function(x) lines(times, exp(-times * x), col = "red"))
lapply(rates_bg0 + rates_0,
       function(x) lines(times, exp(-times * x), col = "blue"))

# means of generation model
lambda_bg <- exp(log(baseline_bg) + beta_bg*mean(age[z == 1]))
lines(times, exp(-times * lambda_bg), col = "red", lty = 3)

lambda_0 <- exp(log(baseline_0) + beta_0*mean(age[z == 2]))
lines(times, exp(-times * (lambda_bg + lambda_0)), col = "blue", lty = 3)

lines(times, exp(-times * lambda_bg)*(cf + (1 - cf)*exp(-times * lambda_0)), lty = 3)


## chains ----
par(mfrow = c(1,1))
xx <-
  as.data.frame(
    extract(res, permuted = FALSE)[, 1, ])
plot(exp(xx$`beta0[1]`),
     exp(xx$`beta_bg[1]`))

# highly correlated...
plot(xx$`beta0[1]`, xx$`beta0[2]`)

plot(xx$`beta_bg[1]`, xx$`beta_bg[2]`)
plot(xx$`beta_bg[1]`, xx$curefrac)
plot(xx$`beta0[1]`, xx$curefrac)

