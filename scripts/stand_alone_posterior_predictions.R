
# stand-alone event time posterior predictions
# Kaplan-Meier plots
#


##TODO: can we avoid looping over posterior samples?
#https://mc-stan.org/docs/2_23/stan-users-guide/stand-alone-generated-quantities-and-ongoing-prediction.html
# res <-
#   rstan::stan(
#     file = here::here("inst/stan/postpred_exp_exp.stan"),
#     data = list(n = 311))


tx_name <- "IPILIMUMAB"
# tx_name <- "NIVOLUMAB"
# tx_name <- "NIVOLUMAB+IPILIMUMAB"

# cf_model <- "cf separate"
cf_model <- "cf pooled"

## stan output
stan_out <-
  readRDS(glue::glue("~/R/rstanbmcm/data/{cf_model}/stan_exp_exp_{tx_name}_.Rds"))
xx <- rstan::extract(stan_out)

# explicitly looping over samples

# # using lambda means
# res <-
#   rstan::stan(
#     file = here::here("inst/stan/postpred_exp_exp.stan"),
#     data = list(n = ncol(xx$lp_os),
#                 n_samples = nrow(xx$beta_os),
#                 curefrac = xx$cf_os,
#                 beta_os= xx$beta_os,
#                 beta_bg = xx$beta_bg),
#     chains = 1,
#     warmup = 0,
#     iter = 1,
#     algorithm = "Fixed_param")


# using lambda case-mix
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
                lambda_pfs_bg = xx$lambda_pfs_bg),
    chains = 1,
    warmup = 0,
    iter = 1,
    algorithm = "Fixed_param")


######################
# Kaplan-Meier plots #
######################

load("~/R/rstanbmcm/data/surv_input_data.RData")
fileloc_out <- glue::glue("plots/post_pred_{cf_model}_exp_exp_{tx_name}.png")
# plot_post_pred_KM(res, tx_name, surv_input_data, fileloc_out)
plot_post_pred_KM(res, tx_name, surv_input_data)

