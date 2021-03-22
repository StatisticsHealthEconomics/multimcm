
#
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
                                   params_joint = list(NA),
                                   centre_age = TRUE,
                                   cf_model = 3,
                                   joint_model = TRUE,
                                   bg_model = 1,
                                   ...) {
  data_os <-
    c(prep_stan_params(model_os, params_os),
      prep_stan_dataTx(input_data,
                       event_type = "OS",
                       centre_age,
                       bg_model))
  
  data_pfs <-
    c(prep_stan_params(model_pfs, params_pfs),
      prep_stan_dataTx(input_data,
                       event_type = "PFS",
                       centre_age,
                       bg_model))
  
  names(data_os) <- paste(names(data_os), "os", sep = "_")
  names(data_pfs) <- paste(names(data_pfs), "pfs", sep = "_")
  
  data_list <-
    c(data_os,
      data_pfs,
      prep_shared_params(params_cf,
                         params_joint,
                         bg_model),
      cf_model = cf_model,
      joint_model = joint_model,
      bg_model = bg_model)
  
  stan_file <-
    if (model_os == "exp") {
      if (model_pfs == "exp") here::here("inst", "stan", "exp_exp_joint_hiercfTx.stan")
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
      # init = list(
      #   list(curefrac = 0.4,
      #        beta_os = c(-3, 0.1),
      #        beta_pfs = c(-3, 0.1),
      #        beta_bg = c(-8, 0.1),
      #        beta_joint = 0.1)),
      chains = chains, ...)
  
  return(res)
}

