# test-boxplots ----


library(purrr)
library(reshape2)
library(dplyr)
library(rstan)
library(shinystan)
library(dplyr)
library(ggplot2)
devtools::load_all()

data("surv_input_data")

all_tx_names <- c("IPILIMUMAB", "NIVOLUMAB", "NIVOLUMAB+IPILIMUMAB")
model_names <- c("exp", "weibull")

stan_fn <- bmcm_joint_stan_file

stan_out <- list()

k <- model_names[1]
i <- model_names[1]

for (j in all_tx_names) {

  stan_out[[j]] <-
    stan_fn(input_data = surv_input_data,
            model_os = k,
            model_pfs = i,
            tx_name = j,
            params_cf = list(mean_beta_cf = 0.9999,
                             var_beta_cf = 0.00001),
            warmup = 1,
            iter = 100,
            thin = 1)
}

curefraction_boxplots(stan_out)


