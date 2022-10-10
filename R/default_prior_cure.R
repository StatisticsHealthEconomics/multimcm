
#' prep shared parameters
#'
#' Model parameters not specific to OS or PFS:
#' cure fraction, background and generated.
#'
#' @param formula_cure
#' @param bg_model Background model. 1: Exponential distribution; 2: fixed point values from life-tables
#'
#' @return list
#'
default_prior_cure <- function(formula_cure,
                               bg_model = 1) {

  ##TODO:
  nTx <- 3
  n_groups <- formula_cure$n_groups
  nvars <- formula_cure$nvars

  mu_alpha_tx <- rep(-0.6, nTx)
  sigma_alpha_tx <- rep(0.8, nTx)

  params_cf <-
    if (formula_cure$cf_idx == 3) {
      list(mu_sd_cf = rep(0, nTx),
           sigma_sd_cf = rep(2.5, nTx),
           mu_alpha = mu_alpha_tx,
           sigma_alpha = sigma_alpha_tx)
    } else if (formula_cure$cf_idx == 2) {
      list(mu_alpha    = matrix(rep(mu_alpha_tx, n_groups),
                                ncol = nTx, byrow = TRUE),
           sigma_alpha = matrix(rep(sigma_alpha_tx, n_groups),
                                ncol = nTx, byrow = TRUE))
    } else if (formula_cure$cf_idx == 1) {
      list(a_cf = 1,
           b_cf = 1)
    }

  params_bg <-
    if (bg_model == 1) {
      list(mu_bg = c(-8.5, 0.03),
           sigma_bg = c(1, 1))
    } else {
      list(mu_bg = numeric(0),
           sigma_bg = numeric(0))}

  params_beta <-
    list(mu_0 = matrix(rep(0, nvars*n_groups),
                       row = n_groups),
         sigma_0 = matrix(rep(1, nvars*n_groups),
                          row = n_groups))

  c(params_cf,
    params_bg,
    params_beta)
}

