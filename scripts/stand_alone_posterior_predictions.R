
# stand-alone event time posterior predictions
# Kaplan-Meier plots
#


##TODO: can we avoid looping over posterior samples?
#https://mc-stan.org/docs/2_23/stan-users-guide/stand-alone-generated-quantities-and-ongoing-prediction.html
# res <-
#   rstan::stan(
#     file = here::here("inst/stan/postpred_exp_exp.stan"),
#     data = list(n = 311))


## stan output
# stan_out <- readRDS("~/R/rstanbmcm/data/cf separate/stan_exp_exp_IPI_.Rds")
# stan_out <- readRDS("~/R/rstanbmcm/data/cf separate/stan_exp_exp_NIVOLUMAB_.Rds")
stan_out <- readRDS("~/R/rstanbmcm/data/cf separate/stan_exp_exp_NIVOLUMAB+IPILIMUMAB_.Rds")
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

orig_data <- load("~/R/rstanbmcm/data/surv_input_data.RData")

tx_name <- "IPILIMUMAB"
fileloc_out <- paste0("plots/post_pred_cfsep_exp_exp_", tx_name, ".png")
plot_post_pred_KM(res, tx_name, orig_data, fileloc_out)

tx_name <- "NIVOLUMAB"
fileloc_out <- paste0("plots/post_pred_cfsep_exp_exp_", tx_name, ".png")
plot_post_pred_KM(res, tx_name, orig_data, fileloc_out)

tx_name <- "NIVOLUMAB+IPILIMUMAB"
fileloc_out <- paste0("plots/post_pred_cfsep_exp_exp_", tx_name, ".png")
plot_post_pred_KM(res, tx_name, orig_data, fileloc_out)

