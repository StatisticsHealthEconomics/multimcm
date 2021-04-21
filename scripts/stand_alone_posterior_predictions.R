
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

# trta <- "IPILIMUMAB"
# trta <- "NIVOLUMAB"
trta <- "NIVOLUMAB+IPILIMUMAB"

cf_idx <- 3
cf_model_names <- c("cf pooled", "cf separate", "cf hier")
cf_model <- cf_model_names[cf_idx]

joint_model <- "independent"
# joint_model <- "joint"

bg_model_idx <- 2
bg_model_names <- c("bg_distn", "bg_fixed")
bg_model <- bg_model_names[bg_model_idx]

model_os_idx <- 1
model_pfs_idx <- 1
model_names <- c("exp", "weibull", "gompertz", "loglogistic", "lognormal", "gengamma")
model_os <- model_names[model_os_idx]
model_pfs <- model_names[model_pfs_idx]


## stan output
stan_out <-
  #   readRDS(glue::glue("~/R/rstanbmcm/data/{joint_model}/{cf_model}/stan_exp_exp_{trta}_.Rds"))
  readRDS(here::here(glue::glue(
    "data/{joint_model}/{cf_model}/{bg_model}/stan_{model_os}_{model_pfs}_{trta}.Rds")))

dat <- rstan::extract(stan_out)


##########
# sample #
##########


# explicitly looping over samples
# using lambda case-mix or means
res <-
  rstan::stan(
    file = here::here("inst/stan/postpred_exp_exp.stan"),
    data = list(n = ncol(dat$lp_os),
                os_model = model_os,
                pfs_model = model_pfs,
                n_samples = nrow(dat$beta_os),
                cf_os = dat$cf_os,
                cf_pfs = dat$cf_pfs,
                lambda_os = array(dat$lambda_os, 1),
                lambda_pfs = array(dat$lambda_pfs, 1),
                lambda_os_bg = dat$lambda_os_bg,
                lambda_pfs_bg = dat$lambda_pfs_bg,
                beta_os = dat$beta_os,
                beta_pfs = dat$beta_pfs,
                beta_bg = dat$beta_bg),
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

load("~/R/rstanbmcm/data/surv_input_data.RData")
fileloc_out <- glue::glue("plots/post_pred_{joint_model}_{cf_model}_exp_exp_{trta}.png")
plot_post_pred_KM(res, trta, surv_input_data, fileloc_out)
plot_post_pred_KM(res, trta, surv_input_data)

fileloc_out <- glue::glue("plots/post_pred_mean_{joint_model}_{cf_model}_exp_exp_{trta}.png")
plot_post_pred_KM(res, trta, surv_input_data, casemix = FALSE, fileloc_out)
plot_post_pred_KM(res, trta, surv_input_data, casemix = FALSE)

