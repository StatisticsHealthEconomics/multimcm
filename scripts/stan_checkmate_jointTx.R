
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

model_os_idx <- 2
model_pfs_idx <- 1
model_names <- c("exp", "weibull", "gompertz", "loglogistic", "lognormal")
model_os <- model_names[model_os_idx]
model_pfs <- model_names[model_pfs_idx]

cf_idx <- 3
cf_model_names <- c("cf pooled", "cf separate", "cf hier")

# all treatments
if (is.na(TRTX)) {
  cf_hier <-
    list(mu_cf_gl = array(-0.8, 1),
         sigma_cf_gl = array(2, 1),
         mu_sd_cf = c(-2, -2, -2),
         sigma_sd_cf = c(1, 1, 1))

  params_cf_lup <-
    list("cf pooled" =
           list(mu_cf_gl = array(-0.8, 1),
                sigma_cf_gl = array(2, 1)),
         "cf separate" =
           list(mu_cf_os = array(-0.8, 1),
                mu_cf_pfs = array(-0.8, 1),
                sd_cf_os = array(0.5, 1),
                sd_cf_pfs = array(0.5, 1)),
         "cf hier" =
           list(exp = cf_hier,
                weibull = cf_hier,
                gompertz = cf_hier,
                loglogistic = cf_hier,
                gengamma = cf_hier,
                lognormal =
                  list(mu_cf_gl = array(-1.8, 1),
                       sigma_cf_gl = array(1, 1),
                       sd_cf_os = c(0.5, 0.5, 0.5),
                       sd_cf_pfs = c(0.5, 0.5, 0.5))))
} else {
  # one treatment only
  # use this to test against single
  # old treatment script
  cf_hier <-
    list(mu_cf_gl = array(-0.8, 1),
         sigma_cf_gl = array(2, 1),
         mu_sd_cf = array(-2, 1),
         sigma_sd_cf = array(1, 1))

  params_cf_lup <-
    list("cf pooled" =
           list(mu_cf_gl = array(-0.8, 1),
                sigma_cf_gl = array(2, 1)),
         "cf separate" =
           list(mu_cf_os = array(-0.8, 1),
                mu_cf_pfs = array(-0.8, 1),
                sd_cf_os = array(0.5, 1),
                sd_cf_pfs = array(0.5, 1)),
         "cf hier" =
           list(exp = cf_hier,
                weibull = cf_hier,
                gompertz = cf_hier,
                loglogistic = cf_hier,
                gengamma = cf_hier,
                lognormal =
                  list(mu_cf_gl = array(-1.8, 1),
                       sigma_cf_gl = array(1, 1),
                       sd_cf_os = array(0.5, 1),
                       sd_cf_pfs = array(0.5, 1))))
}

##TODO: what is this doing?
params_cf <-
  if (is.null(params_cf_lup[[cf_idx]][[model_pfs]])) {
    params_cf_lup[[cf_idx]]
  } else {
    params_cf_lup[[cf_idx]][[model_pfs]]
  }

# cure fraction: 20%, 35%, 45% on logit scale
# no intercept model
# params_tx <-
#   list(mu_alpha = c(-1.4, -0.6, -0.2),
#        sigma_alpha = c(1, 1, 1))
params_tx <- NA

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
  # bmcm_joint_stan_fileTx(
  bmcm_joint_stan_stringTx(
    input_data = surv_input_data,
    model_os = model_os,
    model_pfs = model_pfs,
    params_cf = params_cf,
    params_tx = params_tx,
    cf_model = cf_idx,
    joint_model = FALSE,
    bg_model = bg_model_idx,
    bg_hr = bg_hr,
    t_max = 60,
    warmup = 100,
    iter = 1000,
    thin = 10)

if (save_res) {
  saveRDS(
    out,
    file = glue::glue(
      "data/independent/{cf_model_names[cf_idx]}/{bg_model}_hr{bg_hr}/stan_{model_os}_{model_pfs}.Rds"))}


#########
# plots #
#########

library(survival)

source("R/plot_S_jointTx.R")
source("R/prep_S_dataTx.R")

gg <- plot_S_jointTx(out, annot_cf = FALSE, data = surv_input_data)
gg

ggsave(gg,
       filename = glue::glue(
         "plots/S_plots_{model_os}_{model_pfs}_{cf_model_names[cf_idx]}_{bg_model}_hr{bg_hr}.png"))

