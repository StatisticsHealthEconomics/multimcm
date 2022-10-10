
#' @rdname bmcm_stan
#'
#' jointly estimate all treatments
#' generate Stan code
#' @import rstanarm
#' @importFrom lme4 mkReTrms
#'
bmcm_stan <- function(input_data,
                      formula,
                      cureformula = ~ 1,
                      distns = "exponential",
                      prior_latent = NA,
                      prior_cure = NA,
                      centre_coefs = TRUE,
                      bg_model = c("bg_distn", "bg_fixed"),
                      bg_hr = 1,
                      t_max = 60,
                      ...) {
  call <- match.call()

  rtn_wd <- getwd()
  setwd(here::here("inst/stan"))
  on.exit(setwd(rtn_wd))

  dots <- list(...)

  # validate_distns()

  ####################
  # pre-processing

  distns <- tolower(distns)
  bg_model <- tolower(bg_model)

  bg_model <- match.arg(bg_model)
  bg_model_idx <- which(bg_model == c("bg_distn", "bg_fixed"))

  formula_latent <- parse_formula(formula, input_data)
  formula_cure <- parse_formula(cureformula, input_data)

  # all groups the same distribution
  if (length(distns) == 1) distns <- rep(distns, n_groups)

  # all groups the same parameters
  if (length(params_groups) == 1)
    params_groups <- rep(params_groups, n_groups)

  if (formula_cure$type == "pooled") formula_cure$cf_idx <- 1L
  else if (formula_cure$type == "separate") formula_cure$cf_idx <- 2L
  else if (formula_cure$type == "hierarchical") formula_cure$cf_idx <- 3L

  if (is.na(prior_cure)) prior_cure <- default_prior_cure(formula_cure)

  if (is.na(prior_latent)) prior_latent <- default_prior_latent(distns, formula_latent)

  ###############################
  # construct data
  # priors and hyper-parameters

  tx_names <- unique(input_data$TRTA)
  n_tx <- length(tx_names)
  tx_names <- factor(tx_names, levels = tx_names)
  Tx_dmat <- diag(n_tx)
  tx_params <- c(Tx_dmat = list(Tx_dmat), nTx = n_tx)

  data <- list()

  for (i in seq_len(n_groups)) {

    data[[i]] <-
      prep_stan_data(formula_dat,
                     event_type = i,
                     centre_coefs,     # generalize to other covariates
                     bg_model,
                     bg_hr)

    names(data[[i]]) <- paste(names(data[[i]]), i, sep = "_")
  }

  stan_inputs$data <-
    c(data,
      prior_latent,
      prep_shared_params(prior_cure,
                         params_joint,
                         bg_model, t_max),
      tx_params,
      cf_model = cf_model,
      joint_model = joint_model,
      bg_model = bg_model)

  # sampler parameters
  stan_inputs$warmup <- warmup
  stan_inputs$iter <- iter
  stan_inputs$thin <- thin
  stan_inputs$control <- list(adapt_delta = 0.99,
                              max_treedepth = 20)
  stan_inputs$chains <- chains


  ##############
  # fit model

  browser()

  stan_inputs$model_code <-
    create_stancode(distns, cf_model, joint_model)

  res <- do.call(rstan::stan, stan_inputs)

  return(res)
}

