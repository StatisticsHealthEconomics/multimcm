
# test log-normal cancer time to event
# stan and plots


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
n <- 2000
# n <- 20

cf <- 0.2
z <- rbinom(n, 1, cf) + 1
n_0 <- sum(z == 1)
n_bg <- sum(z == 2)

# age <- rep(c(10,25,50,100), each = n/4)
age <- rep(50, each = n) # all the same age

age_0_centred <- age[z == 1] - mean(age)
age_bg_centred <- age[z == 2] - mean(age)
age_centred <- c(age_bg_centred, age_0_centred)

beta_bg <- c(-8, 0.005)
rates_bg <- exp(beta_bg[1] + beta_bg[2]*age_bg_centred)
times_bg <- rexp(n_bg, rate = rates_bg)

sd_0 <- 1

beta_0 <- c(1, 0.005)
mean_0 <- beta_0[1] + beta_0[2]*age_0_centred
rates_bg2 <- exp(beta_bg[1] + beta_bg[2]*age_0_centred)

times_0 <- rlnorm(n_0, meanlog = mean_0, sdlog = sd_0)
times_bg2 <- rexp(n_0, rate = rates_bg2)
times_bg0 <- pmin(times_0, times_bg2)

times_all <- c(times_bg, times_bg0)
t_max <- 60

fixed_cf <- FALSE

## censoring
# uniform sample then censor selected
# cens_idx <- sample(1:n, size = 50, replace = FALSE)
#
# times_all[cens_idx] <-
#   map_dbl(times_all[cens_idx], function(x) runif(1, 0, x))
#
# cens <- as.numeric(!(1:n) %in% cens_idx)
cens <- rep(1, n)


if (!fixed_cf) {

  data_stan <-
    list(mu_0 = beta_0,
         sigma_0 = c(1,1),
         mu_bg = beta_bg,
         sigma_bg = c(1,1),
         a_sd = 1,
         b_sd = 1,
         a_cf = 3,
         b_cf = 12,
         n = n,
         t = times_all,
         d = cens,
         H = 2,
         X = matrix(c(rep(1, n),
                      age_centred),
                    byrow = FALSE,
                    ncol = 2),
         t_max = t_max)

  stan_object <- stanmodels$exp_relative_mix

} else {

  # small sigma takes a long time for runs
  data_stan <-
    list(beta_0 = beta_0,
         sigma_0 = c(1,1),
         mu_bg = beta_bg,
         sigma_bg = c(1,1),
         a_alpha = 0.1,
         b_alpha = 0.1,
         curefrac = cf,
         n = n,
         t = times_all,
         d = cens,
         H = 2,
         X = matrix(c(rep(1, n),
                      age_centred),
                    byrow = FALSE,
                    ncol = 2),
         t_max = t_max)

  stan_object <- stanmodels$exp_relative_mix
}

# res <-
#   rstan::sampling(
#     stanmodels$lognormal,
#     data = data_beta,
#     warmup = 1000,
#     iter = 10000,
#     thin = 10,
#     control = list(adapt_delta = 0.99,
#                    max_treedepth = 20),
#     chains = 1)

res <-
  rstan::stan(
    file = here::here("inst", "stan", "lognormal_relative_mix.stan"),
    data = data_stan,
    warmup = 100,
    iter = 1000,
    thin = 10,
    control = list(adapt_delta = 0.99,
                   max_treedepth = 20),
    chains = 1)


# rstan::check_divergences(res)


#########
# plots #
#########

library(survival)

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

# par(mfrow = c(1,3))
par(mfrow = c(1,1))

# posteriors
plot(colMeans(fit_stan$S_pred), type = "l", ylim = c(0, 1), lwd = 2.5)
lines(colMeans(fit_stan$S_bg), type = "l", col = "red", lwd = 2.5)
lines(colMeans(fit_stan$S0), type = "l", col = "blue", lwd = 2.5)

# lines(apply(fit_stan$S0, 2, function(x) quantile(x, probs = 0.025)),
# type = "l", col = "blue")


## kaplan-meier

## without censoring
plot(
  survfit(Surv(times_bg, rep(1, n_bg)) ~ 1),
  col = "red",
  xlim = c(0, 5),
  # xlim = c(0, t_max),
  conf.int = F)
lines(
  survfit(Surv(times_0, rep(1, n_0)) ~ 1),
  col = "blue", conf.int = F)
lines(
  survfit(Surv(c(times_bg, times_0), rep(1, n)) ~ 1), conf.int = F)

# ## with censoring
# plot(
#   survfit(Surv(times_all[1:n_bg], cens[1:n_bg]) ~ 1),
#   col = "red",
#   xlim = c(0, t_max), mark.time = TRUE)
# lines(
#   survfit(Surv(times_all[(n_bg + 1):n], cens[(n_bg + 1):n]) ~ 1),
#   col = "blue",
#   mark.time = TRUE)
# lines(
#   survfit(Surv(times_all, cens) ~ 1),
#   mark.time = TRUE)


# for each ages in sample
times <- 0:t_max
plot(NA, type = 'n', xlim = c(0, t_max), ylim = c(0,1))

lapply(rates_bg, function(x) lines(times, exp(-times * x), col = "red"))

purrr::map2(.x = mean_0, .y = sd_0,
            ~ lines(times, 1 - plnorm(times, meanlog = .x, sdlog = .y),
                    col = "blue"))

# means of generation model

# lambda_bg <- exp(beta_bg[1] + beta_bg[2]*mean(age[z == 1]))
# lines(times, exp(-times * lambda_bg), col = "red", lty = 3)
# lambda_0 <- exp(beta_0[1] + beta_bg[2]*mean(age[z == 2]))
# lines(times, exp(-times * (lambda_bg + lambda_0)), col = "blue", lty = 3)
# lines(times, exp(-times * lambda_bg)*(cf + (1 - cf)*exp(-times * lambda_0)), lty = 3)


## chains ----
par(mfrow = c(1,1))
xx <- as.data.frame(extract(res, permuted = FALSE)[, 1, ])
plot(exp(xx$`beta0[1]`), exp(xx$`beta_bg[1]`))

# highly correlated...
plot(xx$`beta0[1]`, xx$`beta0[2]`)

plot(xx$`beta_bg[1]`, xx$`beta_bg[2]`)
plot(xx$`beta_bg[1]`, xx$curefrac)
plot(xx$`beta0[1]`, xx$curefrac)

