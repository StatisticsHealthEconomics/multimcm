
# estimate hazard ratio
# for WHO life-table and
# complete responders


library(purrr)
library(reshape2)
library(dplyr)
library(rstan)
library(shinystan)
library(dplyr)
library(ggplot2)


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)

data("surv_input_data")

# surv_input_data$PFS_rate <- surv_input_data$PFS_rate/12  # months
# surv_input_data$OS_rate <- surv_input_data$OS_rate/12

surv_input_data$pfs <- surv_input_data$pfs/12            # years
surv_input_data$os <- surv_input_data$os/12

# complete responders only
dat <- surv_input_data %>% filter(best_overall_resp == "CR")

# avoid log(0)
dat$OS_rate[dat$OS_rate == 0] <- 1e-20

# alpha prior
# hist(rgamma(400, 15, 10), breaks = 50)

out <-
  rstan::stan(
    file = "inst/stan/lifetable_adjust.stan",
    data = list(
      n = nrow(dat),
      d = dat$os_event,
      S_hat = dat$OS_S,
      h_hat = dat$OS_rate,
      a_alpha = 15,
      b_alpha = 10),
    warmup = 100,
    iter = 1000,
    thin = 10,
    control = list(adapt_delta = 0.99,
                   max_treedepth = 20),
    chains = 2)

res <- extract(out)
hist(res$alpha, breaks = 20)


## test with dummy data

n <- 200
rate1 <- 3 # known times i.e. OS events
rate2 <- 2 # known curve i.e. life-table

times <- rexp(n, rate1)
h_hat <- rep(rate2, n)
S_hat <- exp(-rate2*times)

out <-
  rstan::stan(
    file = "inst/stan/lifetable_adjust.stan",
    data = list(
      n = n,
      d = rep(1, n),
      S_hat = S_hat,
      h_hat = h_hat,
      a_alpha = 1,
      b_alpha = 1),
    warmup = 100,
    iter = 1000,
    thin = 10,
    control = list(adapt_delta = 0.99,
                   max_treedepth = 20),
    chains = 2)

res <- rstan::extract(out)
hist(res$alpha, breaks = 20)




