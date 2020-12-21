

# tests
# 1- shared; 2- separate; 3- hierarchical

# cf separate
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
    params_cf = list(mu_cf_os = -0.8,
                     mu_cf_pfs = -0.8,
                     sd_cf_os = 0.5,
                     sd_cf_pfs = 0.5),
    cf_model = 2,
    joint_model = FALSE,
    warmup = 100,
    iter = 1000,
    thin = 10)



