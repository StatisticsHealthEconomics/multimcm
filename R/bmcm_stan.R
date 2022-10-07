
#' @rdname bmcm_stan
#'
#' jointly estimate all treatments
#' generate Stan code
#' @import rstanarm
#' @importFrom lme4 mkReTrms
#'
bmcm_stan <- function(input_data,
                      formula,
                      distns = "exp",
                      iter = 2000,
                      warmup = 1000,
                      thin = 10,
                      chains = 1,
                      params_groups = NA,
                      params_cf = NA,
                      params_tx = NA,
                      params_joint = list(NA),
                      centre_age = TRUE,
                      cf_model = c("cf pooled", "cf separate", "cf hier"),
                      joint_model = TRUE,
                      bg_model = 1,
                      bg_hr = 1,
                      t_max = 60,
                      ...) {
  rtn_wd <- getwd()
  setwd(here::here("inst/stan"))
  on.exit(setwd(rtn_wd))


  ####################
  # pre-processing

  cf_model <- match.arg(cf_model)

  formula_dat <- parse_formula(formula, input_data)

  n_groups <- length(unique(formula_dat$mf[[formula_dat$group_var]]))

  # all groups the same distribution
  if (length(distns) == 1) distns <- rep(distns, n_groups)

  # all groups the same parameters
  if (length(params_groups) == 1)
    params_groups <- rep(params_groups, n_groups)

  formula_dat$mf[[formula_dat$group_var]] <-
    as.factor(formula_dat$mf[[formula_dat$group_var]])


  ####################
  # construct data

  data <- list()

  for (i in seq_len(n_groups)) {

    data[[i]] <-
      c(prep_distn_params(distns[i], params_groups[i]),
        prep_stan_data(formula_dat,
                       event_type = i,
                       centre_age,  # generalise to other covariates
                       bg_model,
                       bg_hr))

    names(data[[i]]) <- paste(names(data[[i]]), i, sep = "_")
  }

  data_list <-
    c(data,
      prep_shared_params(params_cf,
                         params_joint,
                         bg_model,
                         t_max),
      prep_tx_params(input_data),
      cf_model = cf_model,
      joint_model = joint_model,
      bg_model = bg_model)


  ###############################
  # priors and hyperparameters


  browser()

  ##############
  # fit model

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

