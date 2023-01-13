
# run all distribution combinations
# in bmcm_stan


library(purrr)
library(reshape2)
library(dplyr)
library(rstan)
library(shinystan)
library(dplyr)
library(glue)
library(ggplot2)
library(abind)
library(survival)

options(mc.cores = parallel::detectCores() - 1)
# options(warn = -1)  # suppress warnings
options(warn = 0)

load("data/long_input_data.RData")

distns <- c(
  # "exp",
  "weibull",
  "gompertz",
  "loglogistic",
  "lognormal")
# "gengamma")

for (d1 in distns) {
  for (d2 in distns) {

    # error_msg <- tryCatch({
      out <-
        bmcm_stan(
          input_data = long_input_data,
          formula = "Surv(time=month, event=status) ~ 1 + age_event",
          cureformula = "~ TRTA + (1 | event_idx)",
          family_latent = c(d1, d2),
          prior_latent = NA,
          prior_cure = NA,
          centre_coefs = TRUE,
          bg_model = "bg_fixed",
          bg_hr = 1,
          t_max = 60)

      saveRDS(out, file = glue::glue("data/{out$output@model_name}.Rds"))
    # },
    # warning = function(w) { },
    # error = function(e) e)
  }
}

