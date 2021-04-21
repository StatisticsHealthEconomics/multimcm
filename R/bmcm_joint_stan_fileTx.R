
#' @rdname bmcm_joint_stan_file
#'
#' jointly estimate all treatments
#'
bmcm_joint_stan_fileTx <- function(input_data,
                                   model_os = "exp",
                                   model_pfs = "exp",
                                   iter = 2000,
                                   warmup = 1000,
                                   thin = 10,
                                   chains = 1,
                                   params_os = NA,
                                   params_pfs = NA,
                                   params_cf = NA,
                                   params_tx = NA,
                                   params_joint = list(NA),
                                   centre_age = TRUE,
                                   cf_model = 3,
                                   joint_model = TRUE,
                                   bg_model = 1,
                                   bg_hr = 1,
                                   t_max = 60,
                                   ...) {
  data_os <-
    c(prep_os_params(model_os, params_os),
      prep_stan_dataTx(input_data,
                       event_type = "OS",
                       centre_age,
                       bg_model,
                       bg_hr))
  data_pfs <-
    c(prep_pfs_params(model_pfs, params_pfs),
      prep_stan_dataTx(input_data,
                       event_type = "PFS",
                       centre_age,
                       bg_model,
                       bg_hr))

  names(data_os) <- paste(names(data_os), "os", sep = "_")
  names(data_pfs) <- paste(names(data_pfs), "pfs", sep = "_")

  data_list <-
    c(data_os,
      data_pfs,
      prep_shared_paramsTx(params_cf,
                           params_joint,
                           bg_model,
                           t_max),
      prep_tx_params(input_data, params_tx),
      cf_model = cf_model,
      joint_model = joint_model,
      bg_model = bg_model)

  stan_file <-
    if (model_os == "exp") {
      if (model_pfs == "exp") here::here("inst", "stan", "exp_exp_joint_hiercfTx.stan")
    } else if (model_os == "gompertz") {
      if (model_pfs == "gompertz") here::here("inst", "stan", "gompertz_gompertzTx.stan")
    } else if (model_os == "weibull") {
      if (model_pfs == "weibull") here::here("inst", "stan", "weibull_weibullTx.stan") ##TODO:
    } else if (model_os == "loglogistic") {
      if (model_pfs == "loglogistic") here::here("inst", "stan", "loglogistic_loglogisticTx.stan") ##TODO:
    }

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

