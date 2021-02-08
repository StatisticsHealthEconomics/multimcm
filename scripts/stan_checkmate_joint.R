
# run Stan mixture cure joint model
# CheckMate 067 data set


library(purrr)
library(reshape2)
library(dplyr)
library(rstan)
library(shinystan)
library(dplyr)
library(ggplot2)

# library(rstanbmcm)
# devtools::load_all()
source("R/bmcm_joint_stan_file.R")
source("R/prep_stan_params.R")
source("R/prep_shared_params.R")
source("R/prep_stan_data.R")


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)

# load("~/Documents/R/mixture_cure_model/data/surv_input_data.RData")
data("surv_input_data")


###############
# model setup #
###############

save_res <- TRUE

trta_idx <- 3
all_tx_names <- c("IPILIMUMAB", "NIVOLUMAB", "NIVOLUMAB+IPILIMUMAB")
trta <- all_tx_names[trta_idx]

model_os_idx <- 1
model_pfs_idx <- 1
model_names <- c("exp", "weibull", "gompertz")
model_os <- model_names[model_os_idx]
model_pfs <- model_names[model_pfs_idx]

cf_idx <- 3
cf_model_names <- c("cf pooled", "cf separate", "cf hier")

params_cf <-
  list(list(mu_cf_gl = array(-0.8, 1),
            sigma_cf_gl = array(2, 1)),
       list(mu_cf_os = array(-0.8, 1),
            mu_cf_pfs = array(-0.8, 1),
            sd_cf_os = array(0.5, 1),
            sd_cf_pfs = array(0.5, 1)),
       list(mu_cf_gl = array(-0.8, 1),
            sigma_cf_gl = array(2, 1),
            sd_cf_os = array(0.5, 1),
            sd_cf_pfs = array(0.5, 1)))

bg_model_idx <- 1
bg_model_names <- c("bg_distn", "bg_fixed")
bg_model <- bg_model_names[bg_model_idx]


#######
# run #
#######

out <-
  bmcm_joint_stan_file(
    input_data = surv_input_data,
    model_os = model_os,
    model_pfs = model_pfs,
    tx_name = trta,
    params_pfs = list(mu_0 = c(-3, 0),
                      sigma_0 = c(0.5, 0.01)),
    params_os = list(mu_0 = c(-3, 0),
                     sigma_0 = c(0.4, 1)),
    params_cf = params_cf[[cf_idx]],
    cf_model = cf_idx,                # 1- shared; 2- separate; 3- hierarchical
    joint_model = FALSE,
    bg_model = bg_model_idx,
    warmup = 100,
    iter = 1000,
    thin = 10)


if (save_res)
  saveRDS(out,
          file = glue::glue(
            "data/independent/{cf_model_names[cf_idx]}/stan_{model_os}_{model_pfs}_{trta}.Rds"))

stan_list <- list(out) %>% setNames(trta)


#########
# plots #
#########

library(survival)

source("R/plot_S_joint.R")
source("R/prep_S_data.R")
source("R/plot_prior_predictions.R")

gg <- plot_S_joint(stan_list = stan_list)
gg

# overlay Kaplan-Meier
##TODO: move to function

fit_os <- survfit(Surv(os, os_event) ~ 1,
                  data = filter(surv_input_data, TRTA == trta))
fit_pfs <- survfit(Surv(pfs, pfs_event) ~ 1,
                   data = filter(surv_input_data, TRTA == trta))
km_data <-
  rbind(
    data.frame(Tx = trta,
               event_type = "os",
               time = fit_os$time,
               surv = fit_os$surv),
    data.frame(Tx = trta,
               event_type = "pfs",
               time = fit_pfs$time,
               surv = fit_pfs$surv))

gg + geom_line(aes(x = time, y = surv),
               data = km_data,
               lwd = 1,
               inherit.aes = FALSE) +
  xlim(0, 60)

plot_prior_predictive(out, event_type = "os")
plot_prior_predictive(out, event_type = "pfs")

