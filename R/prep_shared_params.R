
#' prep_shared_params
#'
#' Model parameters not specific to OS or PFS:
#' cure fraction, background and generated.
#'
#' @param cf_params list consisting of potentially:
#'                  - mean_beta_cf Mean of cure fraction to use in method of
#'                                 moments (MoM) to obtain Beta distribution
#'                                 hyper-parameters a, b
#'                  - var_beta_cf Variance of cure fraction for MoM
#'                  - mu_cf_gl Mean of cure fraction to use directly
#'                  - sigma_cf_gl Standard deviation of cure fraction to use directly
#'                  - sd_cf_os, sd_cf_pfs Hierarchical cure fraction standard deviations
#' @param joint_params list: mu_joint, sigma_joint
#' @param mu_bg Mean of background survival
#' @param sigma_bg Standard deviation of background survival
#' @param t_max Time horizon
#'
#' @return list
#'
prep_shared_params <- function(cf_params = NA,
                               joint_params = list(NA),
                               mu_bg = c(-8.5, 0.03),
                               sigma_bg = c(1, 1),
                               t_max = 60) {
  # all cure fraction parameters
  empty_params <-
    list(a_cf = numeric(0),
         b_cf = numeric(0),
         mu_cf_gl = numeric(0),
         mu_cf_os = numeric(0),
         mu_cf_pfs = numeric(0),
         sigma_cf_gl = numeric(0),
         sd_cf_os = numeric(0),
         sd_cf_pfs = numeric(0))

  if (!is.null(cf_params$mean_beta_cf) &&
      !is.null(cf_params$var_beta_cf)) {

    mombeta <-
      MoM_beta(cf_params$mean_beta_cf,
               cf_params$var_beta_cf)
    cf_params <-
      list(a_cf = mombeta$a,
           b_cf = mombeta$b)

  } else if (any(is.na(cf_params))) {
    cf_params <-
      list(a_cf = 1,
           b_cf = 1)}

  empty_joint <-
    list(mu_joint = numeric(0),
         sigma_joint = numeric(0))

  cf_params <-
    modifyList(empty_params,
               cf_params)

  joint_params <-
    modifyList(empty_joint,
               joint_params)

  c(cf_params,
    c(joint_params,
      list(t_max = t_max,
           mu_bg = mu_bg,
           sigma_bg = sigma_bg)))
}

