
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

# library(rstanbmcm)
# devtools::load_all()
source("R/bmcm_joint_stan_fileTx.R")
source("R/prep_stan_params.R")
source("R/prep_shared_paramsTx.R")
source("R/prep_stan_dataTx.R")
source("R/prep_tx_params.R")
source("R/plot_post_pred_KM.R")

source("R/bmcm_joint_stan_stringTx.R")
source("R/create_stancodeTx.R")
source("R/create_block_codeTx.R")


# rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)

data("surv_input_data")


###############
# model setup #
###############

# convert to months
surv_input_data <-
  surv_input_data %>%
  mutate(PFS_rate = PFS_rate/12,
         OS_rate = OS_rate/12)

# remove empty treatment rows
surv_input_data <- surv_input_data[surv_input_data$TRTA != "", ]

## single treatment only?
TRTX <- NA
# TRTX <- "IPILIMUMAB"

if (!is.na(TRTX))
  surv_input_data <- filter(surv_input_data, TRTA == TRTX)

save_res <- TRUE

model_os_idx <- 1
model_pfs_idx <- 1
model_names <- c("exp", "weibull", "gompertz", "loglogistic", "lognormal")
model_os <- model_names[model_os_idx]
model_pfs <- model_names[model_pfs_idx]

cf_idx <- 3
cf_model_names <- c("cf pooled", "cf separate", "cf hier")

# all treatments
if (is.na(TRTX)) {

  params_cf_lup <-
    list("cf pooled" = NULL,
         "cf separate" = NULL,
         "cf hier" =
           list(mu_sd_cf = c(0, 0, 0),
                sigma_sd_cf = c(2.5, 2.5, 2.5)))
} else {
  # single treatment
  # use this to test against single
  # old treatment script

  params_cf_lup <-
    list("cf pooled" = NULL,
         "cf separate" = NULL,
         "cf hier" =
           list(mu_sd_cf = array(0, 1),
                sigma_sd_cf = array(2.5, 1)))
}

params_cf <-
  if (is.null(params_cf_lup[[cf_idx]][[model_pfs]])) {
    params_cf_lup[[cf_idx]]
  } else {
    params_cf_lup[[cf_idx]][[model_pfs]]
  }

# no intercept model
# sum(boot::inv.logit(rnorm(1000, -0.6, 0.8)) > 0.6)/1000
# hist(boot::inv.logit(rnorm(1000, -0.6, 0.8)), breaks = 20, xlim = c(0,1))

# same for all tx
mu_alpha <- c(-0.6, -0.6, -0.6)
sigma_alpha <- c(0.8, 0.8, 0.8)

params_tx <-
  if (cf_idx == 1) {
    list(mu_alpha = mu_alpha,
         sigma_alpha = sigma_alpha)
  } else if (cf_idx == 2) {
    list(mu_alpha_os = mu_alpha,
         sigma_alpha_os = sigma_alpha,
         mu_alpha_pfs = mu_alpha,
         sigma_alpha_pfs = sigma_alpha)
  } else if (cf_idx == 3) {
    list(mu_alpha = mu_alpha,
         sigma_alpha = sigma_alpha)
  } else {
    NA
  }

bg_model_idx <- 2
bg_model_names <- c("bg_distn", "bg_fixed")
bg_model <- bg_model_names[bg_model_idx]

# background hazard ratio
# bg_hr <- 1.63
bg_hr <- 1


#######
# run #
#######

out <-
  bmcm_joint_stan_fileTx(
  # bmcm_joint_stan_stringTx(
    input_data = surv_input_data,
    model_os = model_os,
    model_pfs = model_pfs,
    params_cf = c(params_cf, params_tx),
    cf_model = cf_idx,
    joint_model = FALSE,
    bg_model = bg_model_idx,
    bg_hr = bg_hr,
    t_max = 60,
    chains = 1,
    warmup = 100,
    iter = 500,
    thin = 1)

if (save_res) {
  saveRDS(
    out,
    file = here::here(glue::glue(
      "data/independent/{cf_model_names[cf_idx]}/{bg_model}_hr{bg_hr}/stan_{model_os}_{model_pfs}.Rds")))}


#########
# plots #
#########

library(survival)

source("R/plot_S_jointTx.R")
source("R/prep_S_dataTx.R")

gg <- plot_S_jointTx(out,
                     annot_cf = FALSE,
                     data = surv_input_data)
gg

ggsave(gg,
       filename = here::here(glue::glue(
         "plots/S_plots_{model_os}_{model_pfs}_{cf_model_names[cf_idx]}_{bg_model}_hr{bg_hr}.png")))

