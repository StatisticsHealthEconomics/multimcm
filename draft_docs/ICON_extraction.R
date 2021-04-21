
# extract posteriors for ICON

stan_exp_exp_IPILIMUMAB <- readRDS("C:/Users/Nathan/Documents/R/rstanbmcm/data/independent/cf hier/bg_fixed/stan_exp_exp_IPILIMUMAB.Rds")

xx <- rstan::extract(stan_exp_exp_IPILIMUMAB)

yy <- xx[c("beta_os", "beta_pfs", "lambda_os", "lambda_pfs", "cf_global", "cf_os", "cf_pfs", "S_os", "S_pfs", "S_os_pred", "S_pfs_pred")]

save(yy, file = "data/exp_exp_ipi.RData")

