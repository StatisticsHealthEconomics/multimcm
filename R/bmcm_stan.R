
#' @rdname bmcm_stan
#'
#' jointly estimate all treatments
#' generate Stan code
#' @import rstanarm
#' @importFrom lme4 mkReTrms
#'
bmcm_stan <- function(input_data,
                      formula,
                      distns = exponential(),
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

  formula <- parse_formula(formula, input_data)

  #----- random effects predictor matrices

  has_bars <- as.logical(length(formula$bars))

  # use 'stan_glmer' approach
  if (has_bars) {
    group_unpadded <- lme4::mkReTrms(formula$bars, mf_cpts)
    group <- pad_reTrms(Ztlist = group_unpadded$Ztlist,
                        cnms   = group_unpadded$cnms,
                        flist  = group_unpadded$flist)
    z_cpts <- group$Z
  } else {
    group  <- NULL
    z_cpts <- matrix(0,length(cpts),0)

  }

  ####################
  # construct data

  for (i in seq_len(n_groups)) {

    data[[i]] <-
      c(prep_params(distns[i], params_groups[i]),
        prep_stan_data(input_data,
                       event_type = i,
                       centre_age,
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

