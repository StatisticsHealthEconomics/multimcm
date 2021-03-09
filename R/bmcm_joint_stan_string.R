
#' Run Stan mixture cure joint model from Stan code string
#'
#' Don't use pre-compiled C code.
#'
#' @param input_data Dataframe with all variables
#' @param model_os Distribution name; exp, weibull, gompertz
#' @param model_pfs Distribution name; exp, weibull, gompertz
#' @param tx_name Treatment name; IPILIMUMAB, NIVOLUMAB, NIVOLUMAB+IPILIMUMAB
#' @param iter integer
#' @param warmup Number of iterations burn-in; integer
#' @param chains Number of chains; integer
#' @param params_os List
#' @param params_pfs List
#' @param params_cf List of possible parameters values;
#'                  e.g. mean_beta_cf Mean cure fraction (default U[0,1])
#'                  var_beta_cf Variance of cure fraction
#' @param params_joint list of mu_joint, sigma_joint
#' @param centre_age Logical to centre regression covariate
#' @param cf_model Select cure fraction model. 1: shared; 2: separate ;3: hierarchical
#' @param joint_model Logical. Select joint event time model. TRUE: joint model; FALSE: separate
#' @param bg_model Background model. 1: Exponential distribution; 2: Direct fixed point values from life-table
#' @param ... Additional arguments
#'
#' @import rstan
#' @import dplyr
#' @return stanfit object
#'
bmcm_joint_stan_string <- function(input_data,
                                   model_os = "exp",
                                   model_pfs = "exp",
                                   tx_name = "IPILIMUMAB",
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
      prep_stan_data(input_data,
                     event_type = "OS",
                     tx_name,
                     centre_age,
                     bg_model))

  data_pfs <-
    c(prep_stan_params(model_pfs, params_pfs),
      prep_stan_data(input_data,
                     event_type = "PFS",
                     tx_name,
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

  stancode <-
    create_stancode(model_os, model_pfs, cf_model, joint_model)

  res <-
    rstan::stan(
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

