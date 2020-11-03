
#' Run stan mixture cure joint model
#'
#' Use pre-compiled C code.
#'
#' @param input_data Dataframe with all variables
#' @param model_os Distribution name; exp, weibull, gompertz
#' @param model_pfs Distribution name; exp, weibull, gompertz
#' @param tx_name Treatment name; IPILIMUMAB, NIVOLUMAB, NIVOLUMAB+IPILIMUMAB
#' @param iter int
#' @param warmup Number of iterations burn-in; int
#' @param chains Number of chains; int
#' @param mean_cf Mean cure fraction; default to U[0,1]
#' @param var_cf Variance of cure fraction
#' @param centre_age Logical to centre regression covariate
#' @param ... Additional arguments
#'
#' @import rstan
#' @import dplyr
#'
bmcm_joint_stan <- function(input_data,
                            model_os = "exp",
                            model_pfs = "exp",
                            tx_name = "IPILIMUMAB",
                            iter = 2000,
                            warmup = 1000,
                            thin = 10,
                            chains = 2,
                            mean_cf = NA,
                            var_cf = NA,
                            centre_age = TRUE,
                            ...) {

  data_os <-
    c(prep_stan_params(model_os), #TODO: this could depend on event type with extra argument?
      prep_stan_data(input_data,
                     event_type == "OS",
                     tx_name,
                     centre_age))

  data_pfs <-
    c(prep_stan_params(model_pfs),
      prep_stan_data(input_data,
                     event_type == "PFS",
                     tx_name,
                     centre_age))

  names(data_os) <- paste(names(data_os), "os", sep = "_")
  names(data_pfs) <- paste(names(data_pfs), "pfs", sep = "_")

  data_list <-
    c(data_os,
      data_pfs,
      prep_shared_params(mean_cf, var_cf),
      mu_joint = 0,
      sigma_joint = 0.1)

  stan_model <-
    if (model_os == "exp") {
      if (model_pfs == "exp")      stanmodels$exp_exp_joint_mix
      if (model_pfs == "weibull")  stanmodels$exp_weibull_joint_mix
    } else if (model_os == "weibull") {
      if (model_pfs == "exp")      stanmodels$weibull_exp_joint_mix
      if (model_pfs == "weibull")  stanmodels$weibull_weibull_joint_mix
    }

  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores() - 1)
  # stan_rdump(c("n_obs", "y"), file = "mix.data.R")

  res <-
    rstan::sampling(
      stan_model,
      data = data_list,
      warmup = warmup,
      iter = iter,
      thin = thin,
      control = list(adapt_delta = 0.99,
                     max_treedepth = 20),
      chains = chains, ...)

  return(res)
}

