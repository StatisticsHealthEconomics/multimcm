
#
# test new joint functions
#
#


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)


bmcm_joint <- function(input_data,
                       model_os = "exp",
                       model_pfs = "exp",
                       tx_name = "IPILIMUMAB",
                       iter = 2000,
                       warmup = 1000,
                       thin = 10,
                       chains = 1,
                       params_os = NA,
                       params_pfs = NA,
                       params_cf = list(mean_beta_cf = NA,
                                        var_beta_cf = NA),
                       centre_age = TRUE,
                       ...) {

  data_os <-
    c(prep_stan_params0(model_os, params_os),
      prep_stan_data(input_data,
                     event_type = "OS",
                     tx_name,
                     centre_age))

  data_pfs <-
    c(prep_stan_params0(model_pfs, params_pfs),
      prep_stan_data(input_data,
                     event_type = "PFS",
                     tx_name,
                     centre_age))

  names(data_os) <- paste(names(data_os), "os", sep = "_")
  names(data_pfs) <- paste(names(data_pfs), "pfs", sep = "_")

  distn_names <- c("exp", "weibull", "gompertz")

  data_list <-
    c(data_os,
      data_pfs,
      do.call(prep_shared_params, params_cf),
      distn_os = which(model_os == distn_names),
      distn_pfs = which(model_pfs == distn_names))

  stan_file <- here::here("inst", "stan", "surv_joint_mix.stan")

  res <-
    rstan::stan(
      file = stan_file,
      data = data_list,
      warmup = warmup,
      iter = iter,
      thin = thin,
      control = list(adapt_delta = 0.99,
                     max_treedepth = 20),
      chains = chains, ...)

  return(res)
}


out <-
  bmcm_joint(
    input_data = surv_input_data,
    model_os = "exp",
    model_pfs = "exp",
    tx_name = "NIVOLUMAB",
    params_cf = list(mean_beta_cf = 0.3,
                     var_beta_cf = 0.005),
    warmup = 1,
    iter = 100,
    thin = 1)



