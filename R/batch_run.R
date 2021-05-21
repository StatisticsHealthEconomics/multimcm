
#' batch_run
#' 
#' single treatment model
#'
batch_run <- function(model_os_idx,
                      model_pfs_idx,
                      trta_idx) {
  
  options(mc.cores = parallel::detectCores() - 1)
  
  data("surv_input_data")
  
  surv_input_data$PFS_rate <- surv_input_data$PFS_rate/12  # months
  surv_input_data$OS_rate <- surv_input_data$OS_rate/12
  
  save_res <- TRUE
  
  all_tx_names <- c("IPILIMUMAB", "NIVOLUMAB", "NIVOLUMAB+IPILIMUMAB")
  trta <- all_tx_names[trta_idx]
  
  model_names <- c("exp", "weibull", "gompertz", "loglogistic", "lognormal")
  model_os <- model_names[model_os_idx]
  model_pfs <- model_names[model_pfs_idx]
  
  cf_idx <- 3
  cf_model_names <- c("cf pooled", "cf separate", "cf hier")
  
  cf_hier <-
    list(mu_cf_gl = array(-0.8, 1),
         sigma_cf_gl = array(2, 1),
         sd_cf_os = array(0.5, 1),
         sd_cf_pfs = array(0.5, 1))
  
  params_cf_lup <-
    list("cf pooled" =
           list(mu_cf_gl = array(-0.8, 1),
                sigma_cf_gl = array(2, 1)),
         "cf separate" =
           list(mu_cf_os = array(-0.8, 1),
                mu_cf_pfs = array(-0.8, 1),
                sd_cf_os = array(0.5, 1),
                sd_cf_pfs = array(0.5, 1)),
         "cf hier" =
           list(exp = cf_hier,
                weibull = cf_hier,
                gompertz = cf_hier,
                loglogistic = cf_hier,
                gengamma = cf_hier,
                lognormal =
                  list(mu_cf_gl = array(-1.8, 1),
                       sigma_cf_gl = array(1, 1),
                       sd_cf_os = array(0.5, 1),
                       sd_cf_pfs = array(0.5, 1))))
  
  params_cf <-
    if (is.null(params_cf_lup[[cf_idx]][[model_pfs]])) {
      params_cf_lup[[cf_idx]]
    } else {
      params_cf_lup[[cf_idx]][[model_pfs]]
    }
  
  bg_model_idx <- 2
  bg_model_names <- c("bg_distn", "bg_fixed")
  bg_model <- bg_model_names[bg_model_idx]
  
  # bg_hr <- 1.63
  bg_hr <- 1
  
  out <-
    bmcm_joint_stan_string(
      input_data = surv_input_data,
      model_os = model_os,
      model_pfs = model_pfs,
      tx_name = trta,
      params_cf = params_cf,
      cf_model = cf_idx,
      joint_model = FALSE,
      bg_model = bg_model_idx,
      bg_hr = bg_hr,
      warmup = 100,
      iter = 1000,
      thin = 10)
  
  if (save_res) {
    saveRDS(
      out,
      file = glue::glue(
        "data/independent/{cf_model_names[cf_idx]}/{bg_model}_hr{bg_hr}/stan_{model_os}_{model_pfs}_{trta}.Rds"))}
  
  return()
}

