
#' Batch run
#'
#' all treatments model
#'
batch_runTx <- function(model_idx,
                        data,
                        cf_idx = 3,
                        save_res = FALSE) {

  # options(mc.cores = parallel::detectCores() - 1)

  # convert to months
  surv_input_data <-
    surv_input_data %>%
    mutate(PFS_rate = PFS_rate/12,
           OS_rate = OS_rate/12)

  # remove empty treatment rows
  surv_input_data <- surv_input_data[surv_input_data$TRTA != "", ]

  ## single treatment only?
  TRTX <- NA
  # TRTX <- "IPILIMUMAB"

  if (!is.na(TRTX))
    surv_input_data <- filter(surv_input_data, TRTA == TRTX)

  model_names <- c("exp", "weibull", "gompertz", "loglogistic", "lognormal")
  model_os <- model_names[model_idx$os]
  model_pfs <- model_names[model_idx$pfs]

  cf_model_names <- c("cf pooled", "cf separate", "cf hier")

  # all treatments
  if (is.na(TRTX)) {
    cf_hier <-
      list(mu_sd_cf = c(-2, -2, -2),
           sigma_sd_cf = c(1, 1, 1))

    params_cf_lup <-
      list("cf pooled" = NULL,
           "cf separate" = NULL,
           "cf hier" =
             list(exp = cf_hier,
                  weibull = cf_hier,
                  gompertz = cf_hier,
                  loglogistic = cf_hier,
                  gengamma = cf_hier,
                  lognormal =
                    list(mu_sd_cf = c(0.5, 0.5, 0.5),
                         sigma_sd_cf = c(0.5, 0.5, 0.5))))
  } else {
    # single treatment
    # use this to test against single
    # old treatment script
    cf_hier <-
      list(mu_sd_cf = array(-2, 1),
           sigma_sd_cf = array(1, 1))

    params_cf_lup <-
      list("cf pooled" = NULL,
           "cf separate" = NULL,
           "cf hier" =
             list(exp = cf_hier,
                  weibull = cf_hier,
                  gompertz = cf_hier,
                  loglogistic = cf_hier,
                  gengamma = cf_hier,
                  lognormal =
                    list(mu_sd_cf = array(0.5, 1),
                         sigma_sd_cf = array(0.5, 1))))
  }

  params_cf <-
    if (is.null(params_cf_lup[[cf_idx]][[model_pfs]])) {
      params_cf_lup[[cf_idx]]
    } else {
      params_cf_lup[[cf_idx]][[model_pfs]]
    }

  # cure fraction: 20%, 35%, 45% on logit scale
  # no intercept model
  params_tx <-
    if (cf_idx == 3) {
      list(mu_alpha = c(-1.4, -0.6, -0.2),
           sigma_alpha = c(1, 1, 1))
    } else if (cf_idx == 2) {
      list(mu_alpha_os = c(-1.4, -0.6, -0.2),
           sigma_alpha_pfs = c(1, 1, 1),
           mu_alpha_os = c(-1.4, -0.6, -0.2),
           sigma_alpha_pfs = c(1, 1, 1))
    } else {
      NA
    }

  bg_model_idx <- 2
  bg_model_names <- c("bg_distn", "bg_fixed")
  bg_model <- bg_model_names[bg_model_idx]

  # background hazard ratio
  # bg_hr <- 1.63
  bg_hr <- 1

  out <-
    bmcm_joint_stan_stringTx(
      input_data = surv_input_data,
      model_os = model_os,
      model_pfs = model_pfs,
      params_cf = params_cf,
      params_tx = params_tx,
      cf_model = cf_idx,
      joint_model = FALSE,
      bg_model = bg_model_idx,
      bg_hr = bg_hr,
      t_max = 60,
      warmup = 100,
      iter = 1000,
      thin = 10)

  if (save_res) {
    saveRDS(
      out,
      file = glue::glue(
        "data/stan_{model_os}_{model_pfs}.Rds"))}   # cluster
  # "data/independent/{cf_model_names[cf_idx]}/{bg_model}_hr{bg_hr}/stan_{model_os}_{model_pfs}.Rds"))}

  invisible(out)
}

