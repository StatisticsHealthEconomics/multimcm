
# stand-alone event time posterior predictions
# Kaplan-Meier plots
#


##TODO: can we avoid looping over posterior samples?
#https://mc-stan.org/docs/2_23/stan-users-guide/stand-alone-generated-quantities-and-ongoing-prediction.html
# res <-
#   rstan::stan(
#     file = here::here("inst/stan/postpred_exp_exp.stan"),
#     data = list(n = 311))


# tx_name <- "IPILIMUMAB"
# tx_name <- "NIVOLUMAB"
tx_name <- "NIVOLUMAB+IPILIMUMAB"

cf_model <- "cf separate"
# cf_model <- "cf pooled"

# joint_model <- "independent"
joint_model <- "joint"

## stan output
stan_out <-
  readRDS(glue::glue("~/R/rstanbmcm/data/{joint_model}/{cf_model}/stan_exp_exp_{tx_name}_.Rds"))
xx <- rstan::extract(stan_out)

# explicitly looping over samples
# using lambda case-mix or means
res <-
  rstan::stan(
    file = here::here("inst/stan/postpred_exp_exp.stan"),
    data = list(n = ncol(xx$lp_os),
                n_samples = nrow(xx$beta_os),
                cf_os = xx$cf_os,
                cf_pfs = xx$cf_pfs,
                lambda_os = xx$lambda_os,
                lambda_os_bg = xx$lambda_os_bg,
                lambda_pfs = xx$lambda_pfs,
                lambda_pfs_bg = xx$lambda_pfs_bg,
                beta_os = xx$beta_os,
                beta_pfs = xx$beta_pfs,
                beta_bg = xx$beta_bg),
    chains = 1,
    warmup = 0,
    iter = 1,
    algorithm = "Fixed_param")


######################
# Kaplan-Meier plots #
######################

load("~/R/rstanbmcm/data/surv_input_data.RData")
fileloc_out <- glue::glue("plots/post_pred_{joint_model}_{cf_model}_exp_exp_{tx_name}.png")
plot_post_pred_KM(res, tx_name, surv_input_data, fileloc_out)
plot_post_pred_KM(res, tx_name, surv_input_data)
fileloc_out <- glue::glue("plots/post_pred_mean_{joint_model}_{cf_model}_exp_exp_{tx_name}.png")
plot_post_pred_KM(res, tx_name, surv_input_data, casemix = FALSE, fileloc_out)
plot_post_pred_KM(res, tx_name, surv_input_data, casemix = FALSE)

