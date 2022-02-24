
#' helper function
#'
define_setup <- function(trta,
                         joint_model,
                         cf_idx,
                         bg_model_idx,
                         model_os_idx,
                         model_pfs_idx) {

  cf_model_names <- c("cf pooled", "cf separate", "cf hier")
  cf_model <- cf_model_names[cf_idx]

  bg_model_names <- c("bg_distn", "bg_fixed_hr1")
  bg_model <- bg_model_names[bg_model_idx]

  model_names <- c("exp", "weibull", "gompertz", "loglogistic", "lognormal", "gengamma")
  model_os <- model_names[model_os_idx]
  model_pfs <- model_names[model_pfs_idx]

  list(trta = trta,
       joint_model = joint_model,
       cf_model = cf_model,
       bg_model = bg_model,
       model_os = model_os,
       model_pfs = model_pfs)
}

