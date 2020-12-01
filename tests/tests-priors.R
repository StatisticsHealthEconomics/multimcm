
# test priors ----

# check behaviour for:
#  boundary conditions
#  varying uncertainty


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
model_names <- c("exp", "weibull")#, "gompertz")

stan_fn <- bmcm_joint_stan_file

stan_files <- list()

k <- model_names[1]
i <- model_names[1]
j <- all_tx_names[1]


#################
# cure fraction #
#################

# background
out <- stan_fn(input_data = surv_input_data,
               model_os = k,
               model_pfs = i,
               tx_name = j,
               params_cf = list(mean_beta_cf = 0.9999,
                                var_beta_cf = 0.00001),
               warmup = 1,
               iter = 100,
               thin = 1)

plot_prior_predictive(out, event_type = "os")
plot_prior_predictive(out, event_type = "pfs")

# disease
out <- stan_fn(input_data = surv_input_data,
               model_os = k,
               model_pfs = i,
               tx_name = j,
               params_cf = list(mean_beta_cf = 0.0001,
                                var_beta_cf = 0.00001),
               warmup = 1,
               iter = 100,
               thin = 1)

plot_prior_predictive(out, event_type = "os")
plot_prior_predictive(out, event_type = "pfs")

# maximum uncertainty
out <- stan_fn(input_data = surv_input_data,
               model_os = k,
               model_pfs = i,
               tx_name = j,
               params_cf = list(mean_beta_cf = 0.5,
                                var_beta_cf = 0.2),
               warmup = 1,
               iter = 100,
               thin = 1)

plot_prior_predictive(out, event_type = "os")
plot_prior_predictive(out, event_type = "pfs")


###########
# disease #
###########

# small uncertainty
out <- stan_fn(input_data = surv_input_data,
               model_os = k,
               model_pfs = i,
               tx_name = j,
               params_os = list(mu_0 = c(-3.1, 0),
                                sigma_0 = c(0.1, 0.1)),
               warmup = 1,
               iter = 100,
               thin = 1)

plot_prior_predictive(out, event_type = "os")

# step curve
out <- stan_fn(input_data = surv_input_data,
               model_os = k,
               model_pfs = i,
               tx_name = j,
               params_os = list(mu_0 = c(-2.1, 0),
                                sigma_0 = c(1, 1)),
               warmup = 1,
               iter = 100,
               thin = 1)

plot_prior_predictive(out, event_type = "os")

# shallow curve
out <- stan_fn(input_data = surv_input_data,
               model_os = k,
               model_pfs = i,
               tx_name = j,
               params_os = list(mu_0 = c(-4.1, 0),
                                sigma_0 = c(1, 1)),
               warmup = 1,
               iter = 100,
               thin = 1)

plot_prior_predictive(out, event_type = "os")

