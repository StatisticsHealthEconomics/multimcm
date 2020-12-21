
#' Run Stan mixture cure joint model from Stan file
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
#' @param ... Additional arguments
#'
#' @import rstan
#' @import dplyr
#'
bmcm_joint_stan_file <- function(input_data,
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
                                 ...) {
  data_os <-
    #TODO: this could depend on event type with extra argument?
    c(prep_stan_params(model_os, params_os),
      prep_stan_data(input_data,
                     event_type = "OS",
                     tx_name,
                     centre_age))

  data_pfs <-
    c(prep_stan_params(model_pfs, params_pfs),
      prep_stan_data(input_data,
                     event_type = "PFS",
                     tx_name,
                     centre_age))

  names(data_os) <- paste(names(data_os), "os", sep = "_")
  names(data_pfs) <- paste(names(data_pfs), "pfs", sep = "_")

  data_list <-
    c(data_os,
      data_pfs,
      prep_shared_params(params_cf,
                         params_joint),
      cf_model = cf_model,
      joint_model = joint_model)

  stan_file <-
    if (model_os == "exp") {
      if (model_pfs == "exp")           here::here("inst", "stan", "exp_exp_joint_hiercf.stan")
      else if (model_pfs == "weibull")  here::here("inst", "stan", "exp_weibull_joint_mix.stan")
    } else if (model_os == "weibull") {
      if (model_pfs == "exp")           here::here("inst", "stan", "weibull_exp_joint_mix.stan")
      else if (model_pfs == "weibull")  here::here("inst", "stan", "weibull_weibull_joint_mix.stan")
    }

  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores() - 1)
  # stan_rdump(c("n_obs", "y"), file = "mix.data.R")

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

