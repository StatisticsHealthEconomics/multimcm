
#' @title bmcm_stan
#'
#' @rdname bmcm_stan
#'
#' @description jointly estimate all treatments
#'    generate Stan code
#'
#' @param input_data Long format data frame
#' @param formula R formula object for latent model component
#' @param cureformula R formula object for cure fraction model component
#' @param family_latent Name of distribution, from
#'    "exp", "weibull", "gompertz", "lognormal", "gamma".
#'    Single string or vector of length number of end-points
#' @param prior_latent Optional
#' @param prior_cure Optional
#' @param centre_coefs Logical
#' @param bg_model User supplied distribution or estimated.
#'    In future this will be probabilities or fitted model object.
#' @param bg_varname Background variable name in \code{input_data}
#' @param bg_hr Background hazard ratio adjustment
#' @param t_max Maximum time horizon
#' @param ... Additional parameters
#' @return Stan output as \code{bmcm} class
#'
#' @import rstanarm
#' @importFrom lme4 mkReTrms
#' @export
#'
bmcm_stan <- function(input_data,
                      formula,
                      cureformula = ~ 1,
                      family_latent = "exponential",
                      # family_cure = "logit",
                      prior_latent = NA,
                      prior_cure = NA,
                      centre_coefs = TRUE,
                      bg_model = c("bg_distn", "bg_fixed"),
                      bg_varname = "bg_rate",
                      bg_hr = 1,
                      t_max = 60,
                      ...) {
  call <- match.call()

  rtn_wd <- getwd()
  setwd(here::here("inst/stan"))
  on.exit(setwd(rtn_wd))

  dots <- list(...)

  ####################
  # pre-processing

  family_latent <- tolower(family_latent)
  bg_model <- tolower(bg_model)

  distns <- validate_distns(family_latent)

  bg_model <- match.arg(bg_model)
  bg_model_idx <- which(bg_model == c("bg_distn", "bg_fixed"))

  formula_latent <- parse_formula(formula, input_data, family = distns)
  formula_cure <- parse_formula(cureformula, input_data)

  if (is_pooled(formula_cure)) formula_cure$cf_idx <- 1L
  else if (is_separate(formula_cure)) formula_cure$cf_idx <- 2L
  else if (is_hier(formula_cure)) formula_cure$cf_idx <- 3L

  if (length(distns) == 1) distns <- rep(distns, formula_cure$n_group)

  ###############################
  # construct data
  # priors and hyper-parameters

  if (is.na(prior_cure)) prior_cure <- default_prior_cure(formula_cure)
  if (is.na(prior_latent)) prior_latent <- default_prior_latent(formula_latent,
                                                                formula_cure)

  tx_names <- unique(input_data$TRTA)
  n_tx <- length(tx_names)
  tx_names <- factor(tx_names, levels = tx_names)
  Tx_dmat <- diag(n_tx)
  tx_params <- c(Tx_dmat = list(Tx_dmat), nTx = n_tx)

  stan_data <- c()

  for (i in seq_len(formula_cure$n_groups)) {
    stan_data <- c(
      stan_data,
      prep_latent_data(formula_cure, formula_latent,
                       event_type = i,
                       centre_coefs),
      prep_bg_data(input_data, bg_varname,
                   formula_cure, event_type = i))
  }

  stan_inputs <- list()

  stan_inputs$data <-
    c(stan_data,
      prior_latent,
      prior_cure,
      tx_params,
      cf_model = formula_cure$cf_idx,
      bg_model = bg_model_idx,
      bg_hr = bg_hr)

  ##TODO: define pars explicitly
  ##
  ## '''


  # default sampler parameters
  dots <-
    modifyList(
      dots,
      list(warmup = 100,
           iter = 500,
           thin = 1,
           chains = 1,
           pars = "cf_1",
           control = list(adapt_delta = 0.99,
                          max_treedepth = 20),
           # include = FALSE,
           model_name = "multimcm_model",
           open_progress = TRUE)#,
           # verbose = TRUE)
    )

  ##############
  # fit model

  stan_inputs$model_code <- create_stancode(distns)

  ## for testing
  # writeLines(stan_inputs$model_code, con = here::here("data/stan_model_code.stan"))
  # model_code <- readr::read_file(here::here("data/stan_model_code_test.stan"))
  # stan_inputs$model_code <- model_code

  res <- list()

  res$stan_output <- do.call(rstan::stan, c(stan_inputs, dots))
  res$call <- call
  class(res) <- "bmcm"

  return(res)
}

