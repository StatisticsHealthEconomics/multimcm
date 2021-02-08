
## model_runs

## cf_model:
## 1- pooled; 2- separate; 3- hierarchical
##
## NOTE: wrap optional argument values in array( , 1)
##       to be used by stan


# cf separate
# event time separate
out <-
  bmcm_joint_stan_file(
    input_data = surv_input_data,
    model_os = "exp",
    model_pfs = "exp",
    tx_name = "NIVOLUMAB+IPILIMUMAB",
    params_pfs = list(mu_0 = c(-3, 0),
                      sigma_0 = c(0.5, 0.01)),
    params_os = list(mu_0 = c(-3, 0),
                     sigma_0 = c(0.4, 1)),
    params_cf = list(mu_cf_os = array(-0.8, 1),
                     mu_cf_pfs = array(-0.8, 1),
                     sd_cf_os = array(0.5, 1),
                     sd_cf_pfs = array(0.5, 1)),
    cf_model = 2,
    joint_model = FALSE,
    warmup = 100,
    iter = 1000,
    thin = 10)

# cf pooled
# event time separate
out <-
  bmcm_joint_stan_file(
    input_data = surv_input_data,
    model_os = "exp",
    model_pfs = "exp",
    tx_name = "NIVOLUMAB",
    params_pfs = list(mu_0 = c(-3, 0),
                      sigma_0 = c(0.5, 0.01)),
    params_os = list(mu_0 = c(-3, 0),
                     sigma_0 = c(0.4, 1)),
    params_cf = list(mean_beta_cf = array(0.5, 1),
                     var_beta_cf = array(0.1, 1)),
    cf_model = 1,
    joint_model = FALSE,
    warmup = 100,
    iter = 1000,
    thin = 10)

# cf hierarchical
# event time separate
out <-
  bmcm_joint_stan_file(
    input_data = surv_input_data,
    model_os = "exp",
    model_pfs = "exp",
    tx_name = "NIVOLUMAB+IPILIMUMAB",
    params_pfs = list(mu_0 = c(-3, 0),
                      sigma_0 = c(0.5, 0.01)),
    params_os = list(mu_0 = c(-3, 0),
                     sigma_0 = c(0.4, 1)),
    params_cf = list(mu_cf_gl = array(-0.8, 1),
                     sigma_cf_gl = array(0.5, 1),
                     sd_cf_os = array(0.5, 1),
                     sd_cf_pfs = array(0.5, 1)),
    cf_model = 3,
    joint_model = FALSE,
    warmup = 100,
    iter = 1000,
    thin = 10)

# cf separate
# event time joint distn
out <-
  bmcm_joint_stan_file(
    input_data = surv_input_data,
    model_os = "exp",
    model_pfs = "exp",
    tx_name = "NIVOLUMAB",
    params_pfs = list(mu_0 = c(-3, 0),
                      sigma_0 = c(0.5, 0.01)),
    params_os = list(mu_0 = c(-3, 0),
                     sigma_0 = c(0.4, 1)),
    params_cf = list(mu_cf_os = array(-0.8, 1),
                     mu_cf_pfs = array(-0.8, 1),
                     sd_cf_os = array(0.5, 1),
                     sd_cf_pfs = array(0.5, 1)),
    params_joint = list(mu_joint = array(-0.1, 1),
                        sigma_joint = array(1, 1)),
    cf_model = 2,
    joint_model = 1, #TRUE
    warmup = 500,
    iter = 2000,
    thin = 20)

saveRDS(out, "data/joint/stan_exp_exp_NIVOLUMAB_.Rds")

