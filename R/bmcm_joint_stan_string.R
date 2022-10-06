
#' @rdname bmcm_joint_stan_file
#'
#' jointly estimate all treatments
#' generate Stan code depending on
#' OS, PFS distributions
#'
bmcm_joint_stan_string <- function(input_data,
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
  rtn_wd <- getwd()
  setwd(here::here("inst/stan"))
  on.exit(setwd(rtn_wd))

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
      prep_tx_params(input_data),
      cf_model = cf_model,
      joint_model = joint_model,
      bg_model = bg_model)

  stancode <-
    create_stancodeTx(model_os, model_pfs, cf_model, joint_model)

  res <-
    rstan::stan(
      model_name = glue::glue("{model_os}_os {model_pfs}_pfs"),
      model_code = stancode,
      data = data_list,
      warmup = warmup,
      iter = iter,
      thin = thin,
      control = list(adapt_delta = 0.99,
                     max_treedepth = 20),
      chains = chains, ...)

  return(res)
}

