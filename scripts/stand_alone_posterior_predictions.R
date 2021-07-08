
# stand-alone event time posterior predictions
# Kaplan-Meier plots
#

#https://mc-stan.org/docs/2_23/stan-users-guide/stand-alone-generated-quantities-and-ongoing-prediction.html
# res <-
#   rstan::stan(
#     file = here::here("inst/stan/postpred_exp_exp.stan"),
#     data = list(n = 311))


###################
# select analysis #
###################

source("R/define_setup.R")

inp <-
  define_setup(
    trta = "NIVOLUMAB+IPILIMUMAB",
    joint_model = "independent",
    cf_idx = 3,
    bg_model_idx = 2,
    model_os_idx = 1,
    model_pfs_idx = 1)

stan_out <-
  readRDS(here::here(glue::glue_data(inp,
    "data/{joint_model}/{cf_model}/{bg_model}/stan_{model_os}_{model_pfs}_{trta}.Rds")))

dat <- rstan::extract(stan_out)

stancode_pp <- create_stancode_postpred()

# stancode_pp <-
#   create_stancode_postpred(model_os_idx = 1,
#                            model_pfs_idx = 1)


##########
# sample #
##########

# explicitly looping over samples
# using lambda case-mix or means
res <-
  rstan::stan(
    # file = here::here("inst/stan/postpred_exp_exp.stan"),
    model_code = stancode_pp,
    data = list(n = ncol(dat$lp_os),
                os_model = 1,
                pfs_model = 1,
                n_samples = nrow(dat$beta_os),
                cf_os = dat$cf_os,
                cf_pfs = dat$cf_pfs,
                lambda_os = dat$lambda_os,
                lambda_pfs = dat$lambda_pfs,
                lambda_os_bg = dat$lambda_os_bg,
                lambda_pfs_bg = dat$lambda_pfs_bg,
                beta_os = dat$beta_os,
                beta_pfs = dat$beta_pfs,
                beta_bg = dat$pbeta_bg),
    chains = 1,
    warmup = 0,
    iter = 1,
    algorithm = "Fixed_param")


##TODO:
# # let stan deal with samples
# res <-
#   rstan::stan(
#     file = here::here("inst/stan/postpred_gqs.stan"),
#     data = list(n = ncol(dat$lp_os),
#                 os_model = model_os_idx,
#                 pfs_model = model_pfs_idx,
#                 cf_os = dat$cf_os,
#                 cf_pfs = dat$cf_pfs,
#                 lambda_os = dat$lambda_os,
#                 lambda_pfs = dat$lambda_pfs,
#                 lambda_os_bg = dat$lambda_os_bg,
#                 lambda_pfs_bg = dat$lambda_pfs_bg),
#     chains = 1,
#     warmup = 0,
#     iter = 1,
#     algorithm = "Fixed_param")


################
# Kaplan-Meier #
################

library(survival)
library(dplyr)

source("R/plot_post_pred_KM.R")

load("~/R/rstanbmcm/data/surv_input_data.RData")

fileloc_out <- glue::glue_data(inp, "plots/post_pred_{joint_model}_{cf_model}_exp_exp_{trta}.png")
plot_post_pred_KM(res, inp$trta, surv_input_data, fileloc_out)
plot_post_pred_KM(res, inp$trta, surv_input_data)

fileloc_out <- glue::glue_data(inp, "plots/post_pred_mean_{joint_model}_{cf_model}_exp_exp_{trta}.png")
plot_post_pred_KM(res, inp$trta, surv_input_data, casemix = FALSE, fileloc_out)
plot_post_pred_KM(res, inp$trta, surv_input_data, casemix = FALSE)

