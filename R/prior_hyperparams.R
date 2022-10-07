# prior hyper-parameters


#'
set_params_cf <- function(cf_idx, dist) {
  # all treatments
  params_cf_lup <-
    list("cf pooled" = NULL,
         "cf separate" = NULL,
         "cf hier" =
           list(mu_sd_cf = c(0, 0, 0),
                sigma_sd_cf = c(2.5, 2.5, 2.5)))

  params_cf <-
    if (is.null(params_cf_lup[[cf_idx]][[model_pfs]])) {
      params_cf_lup[[cf_idx]]
    } else {
      params_cf_lup[[cf_idx]][[model_pfs]]
    }

  params_cf
}


#'
set_params_tx <- function(cf_idx) {

  # same for all tx
  mu_alpha <- c(-0.6, -0.6, -0.6)
  sigma_alpha <- c(0.8, 0.8, 0.8)

  params_tx <-
    if (cf_idx == 1) {
      list(mu_alpha = mu_alpha,
           sigma_alpha = sigma_alpha)
    } else if (cf_idx == 2) {
      list(mu_alpha_os = mu_alpha,
           sigma_alpha_os = sigma_alpha,
           mu_alpha_pfs = mu_alpha,
           sigma_alpha_pfs = sigma_alpha)
    } else if (cf_idx == 3) {
      list(mu_alpha = mu_alpha,
           sigma_alpha = sigma_alpha)
    } else {
      NA
    }

  params_tx
}

