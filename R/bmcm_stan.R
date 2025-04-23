
#' @title bmcm_stan
#'
#' @rdname bmcm_stan
#'
#' @description Jointly estimates all treatments and generates Stan code for Bayesian relative mixture cure modeling.
#' It supports various latent and cure model configurations and allows the use of precompiled models or on-the-fly compilation.
#'
#' @param input_data A long-format data frame containing the input data.
#' @param formula An R formula object specifying the latent model component.
#' @param cureformula An R formula object specifying the cure fraction model component. Default is `~ 1`.
#' @param family_latent A string or vector specifying the distribution(s) for the latent model component.
#'    Supported values are `"exp"`, `"weibull"`, `"gompertz"`, `"lognormal"`, `"loglogistic"`, and `"gengamma"`.
#' @param prior_latent An optional prior specification for the latent model component. Default is `NA`.
#' @param prior_cure An optional list specifying priors for the cure fraction model component. Default is an empty list.
#' @param centre_coefs Logical. If `TRUE`, coefficients will be centered. Default is `FALSE`.
#' @param bg_model A string specifying the background model type. Supported values are `"bg_distn"` and `"bg_fixed"`.
#' @param bg_varname A string specifying the background variable name in `input_data`. Default is `"bg_rate"`.
#' @param bg_hr A numeric value for the background hazard ratio adjustment. Default is `1`.
#' @param t_max A numeric value specifying the maximum time horizon for the model. Default is `70`.
#' @param precompiled_model_path A string specifying the path to a precompiled model. Default is `NA`.
#' @param use_cmdstanr Logical. If `TRUE`, the `cmdstanr` package is used to fit the model. Default is `FALSE`.
#' @param save_stan_code Logical. If `TRUE`, saves the Stan model code to a file. Default is `FALSE`.
#' @param read_stan_code Logical. If `TRUE`, reads the Stan model code from a file instead of generating it dynamically. Default is `FALSE`.
#' @param ... Additional parameters to pass to the Stan sampler.
#'
#' @return A list of class `bmcm` containing the Stan model output, MCMC parameters,
#'    function call, distributions used, model inputs, input data, and formulas for the cure and latent components.
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
compile_model <- function(use_cmdstanr,
                          model_code, model_name,
                          file_path = NA) {
  if (is.na(file_path)) {
    file_path <- "."
  }

  if (use_cmdstanr) {
    model_path <-
      cmdstanr::write_stan_file(
        model_code, dir = ".", basename = model_name)
    return(
      cmdstanr::cmdstan_model(
        stan_file = model_path,
        compile = TRUE,
        dir = file_path))
  } else {
    out <- rstan::stan_model(model_code = model_code, model_name = model_name)
    saveRDS(out, file = glue::glue("{file_path}/{model_name}.RDS"))

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
        list(iter_warmup = 100,
             iter_sampling = 500,
             save_warmup = FALSE,
             thin = 1,
             chains = 1,
             adapt_delta = 0.99,
             max_treedepth = 100,
             step_size = 0.05),
        dots
      ))
  } else {
    return(
      modifyList(
        list(warmup = 100,
             iter = 500,
             thin = 1,
             chains = 1,
             control = list(
               adapt_delta = 0.99,
               max_treedepth = 100,
               stepsize = 0.05),
             include = TRUE,
             open_progress = TRUE),
        # verbose = TRUE)
        dots
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
  formula_cure
}
