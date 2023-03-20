
# run Stan mixture cure joint model
# CheckMate 067 data set


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
# library(rstanarm)

# rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)

data("surv_input_data")


#################
# model set-up

save_res <- TRUE

# distn <- c("lognormal", "lognormal")
distn <- "exponential"

load(file = "data/long_input_data.RData")


##############
# run model

out <-
  bmcm_stan(
    input_data = long_input_data,
    formula = "Surv(time=month, event=status) ~ 1 + age_event",
    cureformula = "~ TRTA + event_idx",
    family_latent = distn,
    prior_latent = NA,
    prior_cure = NA,
    centre_coefs = TRUE,
    bg_model = "bg_fixed",
    bg_hr = 1,
    t_max = 60)

if (save_res) {
  save(out, file = glue::glue("data/separate/{out$output@model_name}.RData"))}


##########
# plots

gg <- plot_S_joint(out, add_km = TRUE,
                   annot_cf = FALSE)
gg

# ggsave(gg, filename = glue::glue("plots/separate/{out$output@model_name}_S_plot.png")))

