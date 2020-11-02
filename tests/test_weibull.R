
# test weibull cancer time to event
# stan and plots
#

library(purrr)
library(reshape2)
library(dplyr)
library(rstan)
library(shinystan)
library(dplyr)
library(ggplot2)
# library(rstanbmcm)
devtools::load_all()

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)

# 1: cancer (both)
# 2: cured (background only)

cf <- 0.2
n <- 400
# n <- 20                     #smaller amount of data
z <- rbinom(n, 1, cf) + 1
n_0 <- sum(z == 1)
n_bg <- sum(z == 2)

age <- rep(c(10,25,50,100), each = n/4)

age_0_centred <- age[z == 1] - mean(age)
age_bg_centred <- age[z == 2] - mean(age)
age_centred <- c(age_bg_centred, age_0_centred)

mu_bg <- c(-8, 0.005)
rates_bg <- exp(mu_bg[1] + mu_bg[2]*age_bg_centred)
times_bg <- rexp(n_bg, rate = rates_bg)

alpha0 <- 1#rgamma(1, 0.1, 0.1)

mu_0 <- c(2.5, 0.005)
rates_0 <- exp(mu_0[1] + mu_0[2]*age_0_centred)
rates_bg2 <- exp(mu_bg[1] + mu_bg[2]*age_0_centred)

# is this a bi-weibull?
times_0 <- rweibull(n_0, shape = alpha0, scale = rates_0)
# map_dbl(rates_0, function(x) rweibull(1, shape = alpha0, scale = x)) #check

times_bg2 <- rexp(n_0, rate = rates_bg2)
times_bg0 <- pmin(times_0, times_bg2)

times_all <- c(times_bg, times_bg0)
t_max <- 60

# # time uniform censoring all
# # cens_times <- rep(Inf, n)       # no censoring
# cens_times <- runif(n, 0, t_max)
# times_all <- pmin(times_all, cens_times)
# cens <- as.numeric(times_all != cens_times)

# uniform sample then censor selected
cens_idx <- sample(1:n, size = 50, replace = FALSE)

times_all[cens_idx] <-
  map_dbl(times_all[cens_idx], function(x) runif(1, 0, x))

cens <- as.numeric(!(1:n) %in% cens_idx)

## check exp case
# par(mfrow = c(1,2))
# rate1 <- 0.5
# rate2 <- 19.5
# times1 <- rexp(1000, rate = rate1)
# times2 <- rexp(1000, rate = rate2)
#
# time_min <- pmin(times1, times2)
# hist(time_min, breaks = 50, xlim = c(0,0.5), freq = FALSE)
# hist(rexp(1000, rate = rate1 + rate2),
#      breaks = 50, xlim = c(0,0.5), freq = FALSE)


data_beta <-
  list(mu_0 = mu_0,
       sigma_0 = c(1,1),
       mu_bg = mu_bg,
       sigma_bg = c(1,1),
       a_alpha = 1,
       b_alpha = 1,
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

# small sigma takes a long time for runs
data_fixed <-
  list(mu_0 = mu_0,
       sigma_0 = c(1,1),
       mu_bg = mu_bg,
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

res_beta <-
  rstan::sampling(
    stanmodels$weibull_relative_mix,
    data = data_beta,
    warmup = 1000,
    iter = 10000,
    thin = 10,
    control = list(adapt_delta = 0.99,
                   max_treedepth = 20),
    chains = 1)

# res_fixed <-
#   rstan::sampling(
#     stanmodels$weibull_cf_fixed,
#     data = data_fixed,
#     warmup = 1000,
#     iter = 10000,
#     thin= 10,
#     control = list(adapt_delta = 0.99,
#                    max_treedepth = 20),
#     chains = 1)

# rstan::check_divergences(res)

res <- res_beta
# res <- res_fixed

#########
# plots #
#########

fit_stan <- extract(res)

mean(fit_stan$curefrac)
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
lines(colMeans(fit_stan$S0), type = "l", col = "blue", lwd = 2.5)

# lines(apply(fit_stan$S0, 2, function(x) quantile(x, probs = 0.025)),
# type = "l", col = "blue")

# kaplan-meier
library(survival)

## without censoring
plot(
  survfit(Surv(times_bg, rep(1, n_bg)) ~ 1),
  col = "red",
  xlim = c(0, t_max))
lines(
  survfit(Surv(times_0, rep(1, n_0)) ~ 1),
  col = "blue")
lines(
  survfit(Surv(c(times_bg, times_0), rep(1, n)) ~ 1))

## with censoring
plot(
  survfit(Surv(times_all[1:n_bg], cens[1:n_bg]) ~ 1),
  col = "red",
  xlim = c(0, t_max), mark.time = TRUE)
lines(
  survfit(Surv(times_all[(n_bg + 1):n], cens[(n_bg + 1):n]) ~ 1),
  col = "blue",
  mark.time = TRUE)
lines(
  survfit(Surv(times_all, cens) ~ 1),
  mark.time = TRUE)


# for each ages in sample
times <- 0:t_max
plot(NA, type = 'n', xlim = c(0, t_max), ylim = c(0,1))
lapply(rates_bg, function(x) lines(times, exp(-times * x), col = "red"))
purrr::map2(.x = rates_bg2, .y = rates_0,
            ~ lines(times, exp(-times * .x) * exp(-(times/.y)^alpha0), col = "blue"))

# means of generation model

# lambda_bg <- exp(mu_bg[1] + mu_bg[2]*mean(age[z == 1]))
# lines(times, exp(-times * lambda_bg), col = "red", lty = 3)
# lambda_0 <- exp(mu_0[1] + mu_bg[2]*mean(age[z == 2]))
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

