
#' assume logistic
#'
default_prior_cure <- function(formula_cure) {

  params_tx <- set_params_tx(formula_cure)
  params_cf <- set_params_cf(formula_cure)

  c(params_cf, params_tx)
}


## prior hyper-parameters

#'
set_params_cf <- function(cf_idx, dist) {
  # all treatments
  params_cf_lup <-
    list("cf pooled" = NULL,
         "cf separate" = NULL,
         "cf hier" =
           list(mu_sd_cf = c(0, 0, 0),
                sigma_sd_cf = c(2.5, 2.5, 2.5)))

  ##TODO: refactor
  params_cf <-
    if (is.null(params_cf_lup[[cf_idx]][[model_pfs]])) {
      params_cf_lup[[cf_idx]]
    } else {
      params_cf_lup[[cf_idx]][[model_pfs]]
    }

  params_cf
}


#'
set_params_tx <- function(cf_idx, nTx) {

  # same for all treatments
  mu_alpha <- rep(-0.6, nTx)
  sigma_alpha <- rep(0.8, nTx)

  params_tx <-
    if (cf_idx == 1) {         # pooled
      list(mu_alpha = mu_alpha,
           sigma_alpha = sigma_alpha)
    } else if (cf_idx == 2) {  # separate
      list(mu_alpha = matrix(rep(mu_alpha,2), ncol = nTx, byrow = TRUE),
           sigma_alpha = matrix(rep(sigma_alpha,2), ncol = nTx, byrow = TRUE))
    } else if (cf_idx == 3) {  # hierarchical
      list(mu_alpha = mu_alpha,
           sigma_alpha = sigma_alpha)
    } else {
      NA
    }

  params_tx
}
