
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
                      distns = "exp",
                      params_groups = NA,
                      params_joint = list(NA),
                      centre_age = TRUE,
                      joint_model = TRUE,
                      bg_model = c("bg_distn", "bg_fixed"),
                      bg_hr = 1,
                      t_max = 60,
                      ...) {
  rtn_wd <- getwd()
  setwd(here::here("inst/stan"))
  on.exit(setwd(rtn_wd))

  ####################
  # pre-processing

  distns <- tolower(distns)

  cf_model <- match.arg(cf_model)
  cf_model_idx <- which(cf_model == c("cf pooled", "cf separate", "cf hier"))

  bg_model <- match.arg(bg_model)
  bg_model_idx <- which(bg_model == c("bg_distn", "bg_fixed"))

  formula_dat <- parse_formula(formula, input_data)

  n_groups <- length(unique(formula_dat$mf[[formula_dat$group_var]]))

  # all groups the same distribution
  if (length(distns) == 1) distns <- rep(distns, n_groups)

  # all groups the same parameters
  if (length(params_groups) == 1)
    params_groups <- rep(params_groups, n_groups)

  formula_dat$mf[[formula_dat$group_var]] <-
    as.factor(formula_dat$mf[[formula_dat$group_var]])


  ###############################
  # construct data
  # priors and hyper-parameters

  params_tx <- set_params_tx(cf_idx)
  params_cf <- set_params_cf(cf_idx, distns)  ##TODO: can't pass distns

  data <- list()

  for (i in seq_len(n_groups)) {

    data[[i]] <-
      c(prep_distn_params(distns[i], params_groups[i]),
        prep_stan_data(formula_dat,
                       event_type = i,
                       centre_age,     # generalize to other covariates
                       bg_model,
                       bg_hr))

    names(data[[i]]) <- paste(names(data[[i]]), i, sep = "_")
  }

  data_list <-
    c(data,
      prep_shared_params(c(params_cf,
                           params_tx),
                         params_joint,
                         bg_model,
                         t_max),
      prep_tx_params(input_data),
      cf_model = cf_model,
      joint_model = joint_model,
      bg_model = bg_model)


  ##############
  # fit model

  browser()

  stancode <-
    create_stancode(distns, cf_model, joint_model)

  res <-
    rstan::stan(
      # model_name = ,
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

