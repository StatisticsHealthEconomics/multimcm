
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
# devtools::load_all()

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


### hierarchical cure fraction

## exponential

out <-
  bmcm_joint_stan_file(
    input_data = surv_input_data,
    model_os = "exp",
    model_pfs = "exp",
    tx_name = "IPILIMUMAB",
    params_pfs = list(mu_0 = c(-3, 0),
                      sigma_0 = c(0.5, 0.001)),
    params_os = list(mu_0 = c(-3, 0),
                     sigma_0 = c(0.4, 1)),
    params_cf = list(mu_cf = -0.8,
                     sigma_cf = 0.1,
                     sd_cf_os = 0.005,
                     sd_cf_pfs = 0.005),
    algorithm = "Fixed_param",
    warmup = 1,
    iter = 100,
    thin = 1)

plot_prior_predictive(out, event_type = "pfs")
plot_prior_predictive(out, event_type = "os")


## loglogistic

out <-
  bmcm_joint_stan_file(
    input_data = surv_input_data,
    model_os = "loglogistic",
    model_pfs = "loglogistic",
    tx_name = "IPILIMUMAB",
    params_pfs = list(a_shape = 1,
                      b_shape = 1,
                      mu_0 = c(2, 0),
                      sigma_0 = c(0.5, 0.01)),
    params_os = list(a_shape = 0.5,
                     b_shape = 0.5,
                     mu_0 = c(-3, 0),
                     sigma_0 = c(0.4, 1)),
    params_cf = list(mu_cf_gl = array(-0.8, 1),
                     sigma_cf_gl = array(2, 1),
                     sd_cf_os = array(0.5, 1),
                     sd_cf_pfs = array(0.5, 1)),
    cf_model = 3,
    joint_model = FALSE,
    bg_model = 2,
    algorithm = "Fixed_param",
    warmup = 1,
    iter = 100,
    thin = 1)

plot_prior_predictive(out, event_type = "pfs")
plot_prior_predictive(out, event_type = "os")


## gompertz

out <-
  bmcm_joint_stan_file(
    input_data = surv_input_data,
    model_os = "gompertz",
    model_pfs = "gompertz",
    tx_name = "IPILIMUMAB",
    params_pfs = list(a_shape = 1,
                      b_shape = 1000,
                      mu_0 = c(-3, 0),
                      sigma_0 = c(0.5, 0.01)),
    params_os = list(a_shape = 0.5,
                     b_shape = 0.5,
                     mu_0 = c(-3, 0),
                     sigma_0 = c(0.4, 1)),
    params_cf = list(mu_cf_gl = array(-0.8, 1),
                     sigma_cf_gl = array(2, 1),
                     sd_cf_os = array(0.5, 1),
                     sd_cf_pfs = array(0.5, 1)),
    cf_model = 3,
    joint_model = FALSE,
    bg_model = 2,
    algorithm = "Fixed_param",
    warmup = 1,
    iter = 100,
    thin = 1)

plot_prior_predictive(out, event_type = "pfs")
plot_prior_predictive(out, event_type = "os")

## log-normal

out <-
  bmcm_joint_stan_file(
    input_data = surv_input_data,
    model_os = "lognormal",
    model_pfs = "lognormal",
    tx_name = "IPILIMUMAB",
    params_pfs = list(a_sd = 1,
                      b_sd = 2,
                      mu_0 = c(1.5, 0),
                      sigma_0 = c(0.5, 0.01)),
    params_os = list(a_sd = 2,
                     b_sd = 1,
                     mu_0 = c(2.5, 0),
                     sigma_0 = c(0.4, 1)),
    params_cf = list(mu_cf_gl = array(-1.8, 1),
                     sigma_cf_gl = array(1, 1),
                     sd_cf_os = array(0.5, 1),
                     sd_cf_pfs = array(0.5, 1)),
    cf_model = 3,
    joint_model = FALSE,
    bg_model = 2,
    algorithm = "Fixed_param",
    warmup = 1,
    iter = 100,
    thin = 1)

plot_prior_predictive(out, event_type = "pfs")
plot_prior_predictive(out, event_type = "os")

## generalised gamma

out <-
  bmcm_joint_stan_file(
    input_data = surv_input_data,
    model_os = "gengamma",
    model_pfs = "gengamma",
    tx_name = "IPILIMUMAB",
    params_pfs = list(a_mu = 1,
                      b_mu = 1,
                      a_Q = 2,
                      b_Q = 1,
                      mu_0 = c(-3, 0),
                      sigma_0 = c(0.5, 0.01)),
    params_os = list(a_mu = 1,
                     b_mu = 1,
                     a_Q = 1,
                     b_Q = 1,
                     mu_0 = c(-3, 0),
                     sigma_0 = c(0.4, 1)),
    params_cf = list(mu_cf_gl = array(-0.1, 1),
                     sigma_cf_gl = array(1, 1),
                     sd_cf_os = array(1, 1),
                     sd_cf_pfs = array(1, 1)),
    cf_model = 3,
    joint_model = FALSE,
    bg_model = 2,
    algorithm = "Fixed_param",
    warmup = 1,
    iter = 100,
    thin = 1)

plot_prior_predictive(out, event_type = "pfs")
plot_prior_predictive(out, event_type = "os")


