
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
#'    "exp", "weibull", "gompertz", "lognormal", "loglogistic", "gengamma".
#'    Single string or vector of length number of end-points
#' @param prior_latent Optional
#' @param prior_cure Optional
#' @param centre_coefs Logical
#' @param bg_model User supplied distribution or estimated.
#'    In future this will be probabilities or fitted model object.
#' @param bg_varname Background variable name in `input_data`
#' @param bg_hr Background hazard ratio adjustment
#' @param t_max Maximum time horizon
#' @param precompiled_model_path Path to precompiled model, Default is NA
#' @param use_cmdstanr Logical. If TRUE, use cmdstanr to fit the model. Default is FALSE.
#' @param ... Additional parameters to pass to the Stan sampler
#' @return Stan output as `bmcm` class
#'
#' @import rstanarm
#' @importFrom lme4 mkReTrms
#' @importFrom glue glue_collapse
#' @export
#'
bmcm_stan <- function(input_data,
                      formula,
                      cureformula = ~ 1,
                      family_latent = "exponential",
                      # family_cure = "logit",
                      prior_latent = NA,
                      prior_cure = list(),
                      centre_coefs = FALSE,
                      bg_model = c("bg_distn", "bg_fixed"),
                      bg_varname = "bg_rate",
                      bg_hr = 1,
                      t_max = 70,
                      save_stan_code = FALSE,
                      read_stan_code = FALSE,
                      precompiled_model_path = NA,
                      use_cmdstanr = FALSE,
                      ...) {
  call <- match.call()
  rtn_wd <- getwd()
  new_wd <- system.file("stan", package = "multimcm")
  setwd(new_wd)
  on.exit(setwd(rtn_wd), add = TRUE)

  dots <- list(...)

  if (!is.na(precompiled_model_path) && !file.exists(precompiled_model_path)) {
    stop("Precompiled Stan model file does not exist at the specified path.")
  }

  ####################
  # pre-processing

  family_latent <- tolower(family_latent)
  bg_model <- tolower(bg_model)

  distns <- validate_distns(family_latent)

  bg_model <- match.arg(bg_model)
  bg_model_idx <- which(bg_model == c("bg_distn", "bg_fixed"))

  formula_cure <- parse_formula(cureformula, input_data)

  # adjust the number of distributions based on the cure model
  if (length(distns) == 1) {
    if (is_hier(formula_cure)) {
      distns <- rep(distns, formula_cure$re_nlevels[1])
    } else if (is_separate(formula_cure)) {
      distns <- rep(distns, formula_cure$fe_nlevels[2])
    }}

  formula_latent <- parse_formula(formula, input_data, family = distns)
  formula_cure <- switch_cure_model_type(formula_cure)

  ###############################
  # construct data
  # priors and hyper-parameters

  prior_cure <- default_prior_cure(formula_cure, prior_cure)

  if (any(is.na(prior_latent)))
    prior_latent <- default_prior_latent(formula_latent, formula_cure)

  tx_names <- unique(input_data[[formula_cure$fe_vars[1]]])
  n_tx <- length(tx_names)
  tx_names <- factor(tx_names, levels = tx_names)
  Tx_dmat <- diag(n_tx)
  tx_params <- c(Tx_dmat = list(Tx_dmat), nTx = n_tx)

  stan_data <- c()

  for (i in seq_len(formula_cure$n_groups)) {
    stan_data <- c(
      stan_data,
      prep_latent_data(input_data,
                       formula_cure, formula_latent,
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
      t_max = t_max,   ##TODO: why was this commented out?
      bg_hr = bg_hr)

  model_name <- paste0("bmcm_stan_", glue::glue_collapse(distns, sep = "_"))

  mcmc_params <- get_stan_defaults(use_cmdstanr, dots)

  ##############
  # fit model

  use_precompiled_model <- !is.na(precompiled_model_path)

  if (use_precompiled_model) {
    precompiled_model <-
      load_precompiled_model(
        use_cmdstanr = use_cmdstanr,
        path = precompiled_model_path)
  } else {
    model_code <-
      get_model_code(read_stan_code, distns)

    precompiled_model <-
      compile_model(
        use_cmdstanr = use_cmdstanr,
        model_code = model_code,
        model_name = model_name)
  }

  # for testing
  if (save_stan_code) {
    writeLines(model_code, con = here::here("data/stan_model_code.stan"))
  }

  res <- list()

  res$output <- perform_sampling(use_cmdstanr, precompiled_model,
                                 stan_inputs, mcmc_params)
  res$mcmc_params <- mcmc_params
  res$call <- call
  res$distns <- distns
  res$inputs <- stan_inputs
  res$input_data <- input_data
  res$formula <- list(cure = formula_cure,
                      latent = formula_latent)
  class(res) <- "bmcm"

  return(res)
}

# helper functions

#
get_model_code <- function(read_stan_code, distns) {
  if (read_stan_code) {
    return(readr::read_file(here::here("data/stan_model_code.stan")))
  } else {
    return(create_stancode(distns))
  }
}

#
compile_model <- function(use_cmdstanr, model_code, model_name) {
  if (use_cmdstanr) {
    model_path <-
      cmdstanr::write_stan_file(
        model_code, dir = ".", basename = model_name)
    return(cmdstanr::cmdstan_model(stan_file = model_path, compile = TRUE))
  } else {
    out <- rstan::stan_model(model_code = model_code, model_name = model_name)
    saveRDS(precompiled_model, file = glue::glue("{model_name}.RDS"))

    return(out)
  }
}

#
load_precompiled_model <- function(use_cmdstanr, path) {
  if (use_cmdstanr) {
    return(cmdstanr::cmdstan_model(exe_file = path))
  } else {
    return(readRDS(path))
  }
}

#
perform_sampling <- function(use_cmdstanr, stan_model,
                             stan_inputs, dots) {
  if (use_cmdstanr) {

    if (!inherits(stan_model, "CmdStanModel")) {
      stop("The 'stan_model' argument must be a CmdStanModel object.")
    }

    output <- do.call(
      stan_model$sample,
      args = c(stan_inputs, dots)
    )
  } else {

    if (!inherits(stan_model, "stanmodel")) {
      stop("The 'stan_model' argument must be a stanmodel object.")
    }

    output <- do.call(
      rstan::sampling,
      args = c(list(object = stan_model),
               stan_inputs, dots)
    )
  }

  output
}

# default sampler parameters
#TODO: cmdstanr and rstan are slightly different
#      harmonize automatically
get_stan_defaults <- function(use_cmdstanr, dots) {
  if (use_cmdstanr) {
    return(
      modifyList(
        dots,
        list(iter_warmup = 100,
             iter_sampling = 500,
             save_warmup = FALSE,
             thin = 1,
             chains = 1,
             adapt_delta = 0.99,
             max_treedepth = 100,
             step_size = 0.05)
      ))
  } else {
    return(
      modifyList(
        dots,
        list(warmup = 100,
             iter = 500,
             thin = 1,
             chains = 1,
             control = list(
               adapt_delta = 0.99,
               max_treedepth = 100,
               stepsize = 0.05),
             include = TRUE,
             open_progress = TRUE)
        # verbose = TRUE)
      ))
  }
}


# identify cure model type
switch_cure_model_type <- function(formula_cure) {
  if (is_pooled(formula_cure)) {
    formula_cure$cf_idx <- 1L
    formula_cure$cf_name <- "pooled"
  } else if (is_separate(formula_cure)) {
    formula_cure$cf_idx <- 2L
    formula_cure$cf_name <- "separate"
  } else if (is_hier(formula_cure)) {
    formula_cure$cf_idx <- 3L
    formula_cure$cf_name <- "hier"
  }
  return(formula_cure)
}
