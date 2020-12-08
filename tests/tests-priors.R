
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

# load("~/R/rstanbmcm/data/surv_input_data.RData")
data("surv_input_data")

all_tx_names <- c("IPILIMUMAB", "NIVOLUMAB", "NIVOLUMAB+IPILIMUMAB")
model_names <- c("exp", "weibull")#, "gompertz")

stan_fn <- bmcm_joint_stan_file

stan_files <- list()


# exp ---------------------------------------------------------------------

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
               algorithm = "Fixed_param",
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
               algorithm = "Fixed_param",
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
               algorithm = "Fixed_param",
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
               algorithm = "Fixed_param",
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
               algorithm = "Fixed_param",
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
               algorithm = "Fixed_param",
               warmup = 1,
               iter = 100,
               thin = 1)

plot_prior_predictive(out, event_type = "os")


#####################
# likelihood-based  #
#####################

# nivo
# pfs
out <- stan_fn(input_data = surv_input_data,
               model_os = k,
               model_pfs = i,
               tx_name = j,
               params_pfs = list(mu_0 = c(-2, 0),
                                sigma_0 = c(0.5, 1)),
               params_cf = list(mean_beta_cf = 0.30,
                                var_beta_cf = 0.001),
               algorithm = "Fixed_param",
               warmup = 1,
               iter = 100,
               thin = 1)

plot_prior_predictive(out, event_type = "pfs")

# ipi
# pfs
out <- stan_fn(input_data = surv_input_data,
               model_os = k,
               model_pfs = i,
               tx_name = j,
               params_pfs = list(mu_0 = c(-2, 0),
                                 sigma_0 = c(0.5, 1)),
               params_cf = list(mean_beta_cf = 0.1,
                                var_beta_cf = 0.001),
               algorithm = "Fixed_param",
               warmup = 1,
               iter = 100,
               thin = 1)

plot_prior_predictive(out, event_type = "pfs")

# niov+ipi
# pfs
out <- stan_fn(input_data = surv_input_data,
               model_os = k,
               model_pfs = i,
               tx_name = j,
               params_pfs = list(mu_0 = c(-2, 0),
                                 sigma_0 = c(0.5, 1)),
               params_cf = list(mean_beta_cf = 0.4,
                                var_beta_cf = 0.001),
               algorithm = "Fixed_param",
               warmup = 1,
               iter = 100,
               thin = 1)

plot_prior_predictive(out, event_type = "pfs")


# nivo
# os
out <- stan_fn(input_data = surv_input_data,
               model_os = k,
               model_pfs = i,
               tx_name = j,
               params_os = list(mu_0 = c(-3, 0),
                                 sigma_0 = c(0.4, 1)),
               params_cf = list(mean_beta_cf = 0.4,
                                var_beta_cf = 0.005),
               algorithm = "Fixed_param",
               warmup = 1,
               iter = 100,
               thin = 1)

plot_prior_predictive(out, event_type = "os")

# ipi
# os
out <- stan_fn(input_data = surv_input_data,
               model_os = k,
               model_pfs = i,
               tx_name = j,
               params_pfs = list(mu_0 = c(-3, 0),
                                 sigma_0 = c(0.4, 1)),
               params_cf = list(mean_beta_cf = 0.2,
                                var_beta_cf = 0.001),
               algorithm = "Fixed_param",
               warmup = 1,
               iter = 100,
               thin = 1)

plot_prior_predictive(out, event_type = "pfs")

# niov+ipi
# os
out <- stan_fn(input_data = surv_input_data,
               model_os = k,
               model_pfs = i,
               tx_name = j,
               params_pfs = list(mu_0 = c(-3, 0),
                                 sigma_0 = c(0.5, 1)),
               params_cf = list(mean_beta_cf = 0.4,
                                var_beta_cf = 0.001),
               algorithm = "Fixed_param",
               warmup = 1,
               iter = 100,
               thin = 1)

plot_prior_predictive(out, event_type = "pfs")


# weibull -----------------------------------------------------------------

# big step in CrI
out <-
  bmcm_joint_stan_file(
    input_data = surv_input_data,
    model_os = "exp",
    model_pfs = "weibull",
    tx_name = "IPILIMUMAB",
    params_pfs = list(a_alpha = 0.1,
                      b_alpha = 0.1,
                      mu_0 = c(3, 0),
                      sigma_0 = c(0.1, 0.01)),
    params_cf = list(mean_beta_cf = 0.5,
                     var_beta_cf = 0.00001),
    algorithm = "Fixed_param",
    warmup = 1,
    iter = 100,
    thin = 1)

plot_prior_predictive(out, event_type = "pfs")

# pfs
# nivo+ipi
out <-
  bmcm_joint_stan_file(
    input_data = surv_input_data,
    model_os = "exp",
    model_pfs = "weibull",
    tx_name = "IPILIMUMAB",
    params_pfs = list(a_alpha = 5,
                      b_alpha = 3,
                      mu_0 = c(2, 0),
                      sigma_0 = c(0.7, 0.01)),
    params_cf = list(mean_beta_cf = 0.4,
                     var_beta_cf = 0.001),
    algorithm = "Fixed_param",
    warmup = 1,
    iter = 100,
    thin = 1)

plot_prior_predictive(out, event_type = "pfs")

# ipi
out <-
  bmcm_joint_stan_file(
    input_data = surv_input_data,
    model_os = "exp",
    model_pfs = "weibull",
    tx_name = "IPILIMUMAB",
    params_pfs = list(a_alpha = 5,
                      b_alpha = 3,
                      mu_0 = c(2, 0),
                      sigma_0 = c(0.5, 0.01)),
    params_cf = list(mean_beta_cf = 0.1,
                     var_beta_cf = 0.001),
    algorithm = "Fixed_param",
    warmup = 1,
    iter = 100,
    thin = 1)

plot_prior_predictive(out, event_type = "pfs")


# nivo
out <-
  bmcm_joint_stan_file(
    input_data = surv_input_data,
    model_os = "exp",
    model_pfs = "weibull",
    tx_name = "IPILIMUMAB",
    params_pfs = list(a_alpha = 5,
                      b_alpha = 3,
                      mu_0 = c(2, 0),
                      sigma_0 = c(0.5, 0.01)),
    params_cf = list(mean_beta_cf = 0.3,
                     var_beta_cf = 0.001),
    algorithm = "Fixed_param",
    warmup = 1,
    iter = 100,
    thin = 1)

plot_prior_predictive(out, event_type = "pfs")


# os
# nivo+ipi
out <-
  bmcm_joint_stan_file(
    input_data = surv_input_data,
    model_os = "exp",
    model_pfs = "weibull",
    tx_name = "IPILIMUMAB",
    params_pfs = list(a_alpha = 5,
                      b_alpha = 3,
                      mu_0 = c(3, 0),
                      sigma_0 = c(0.5, 0.01)),
    params_cf = list(mean_beta_cf = 0.5,
                     var_beta_cf = 0.001),
    algorithm = "Fixed_param",
    warmup = 1,
    iter = 100,
    thin = 1)

plot_prior_predictive(out, event_type = "pfs")

# ipi
out <-
  bmcm_joint_stan_file(
    input_data = surv_input_data,
    model_os = "exp",
    model_pfs = "weibull",
    tx_name = "IPILIMUMAB",
    params_pfs = list(a_alpha = 5,
                      b_alpha = 3,
                      mu_0 = c(3, 0),
                      sigma_0 = c(0.5, 0.01)),
    params_cf = list(mean_beta_cf = 0.2,
                     var_beta_cf = 0.001),
    algorithm = "Fixed_param",
    warmup = 1,
    iter = 100,
    thin = 1)

plot_prior_predictive(out, event_type = "pfs")

# nivo
out <-
  bmcm_joint_stan_file(
    input_data = surv_input_data,
    model_os = "exp",
    model_pfs = "weibull",
    tx_name = "IPILIMUMAB",
    params_pfs = list(a_alpha = 5,
                      b_alpha = 3,
                      mu_0 = c(3, 0),
                      sigma_0 = c(0.5, 0.01)),
    params_cf = list(mean_beta_cf = 0.5,
                     var_beta_cf = 0.001),
    algorithm = "Fixed_param",
    warmup = 1,
    iter = 100,
    thin = 1)

plot_prior_predictive(out, event_type = "pfs")

