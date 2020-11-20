
#' prep_shared_params
#'
#' Model parameters not specific to OS or PFS:
#' cure fraction, background and generated.
#'
#' @param mean_cf Mean of cure fraction to using in method of
#'                moments (MoM) to obtain Beta distribution
#'                hyper-parameters a, b
#' @param var_cf Variance of cure fraction for MoM
#' @param mu_bg Mean of background survival
#' @param sigma_bg Standard deviation of background survival
#' @param t_max Time horizon
#' @param mu_cf Mean of cure fraction to use directly
#' @param sigma_cf Standard deviation of cure fraction to use directly
#'
#' @return list
#'
prep_shared_params <- function(mean_cf = NA,
                               var_cf = NA,
                               mu_cf = NA,
                               sigma_cf = NA,
                               mu_bg = c(-8.5, 0.03),
                               sigma_bg = c(1, 1),
                               t_max = 60) {
  # cure fraction parameters
  cf_params <-
    if (!is.na(mean_cf) && !is.na(var_cf)) {
      mombeta <- MoM_beta(mean_cf, var_cf)
      list(a_cf = mombeta$a,
           b_cf = mombeta$b)
    } else if (all(!is.na(mu_cf)) &&
               all(!is.na(sigma_cf))) {
      list(mu_cf = mu_cf,
           sigma_cf = sigma_cf)
    } else {
      list(a_cf = 1, b_cf = 1)}

  c(cf_params,
    list(t_max = t_max,
         mu_bg = mu_bg,
         sigma_bg = sigma_bg))
}

