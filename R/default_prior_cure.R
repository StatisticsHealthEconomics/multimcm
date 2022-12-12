
#' Prepare shared parameters for Stan model
#'
#' Model parameters for cure fraction, background and generated.
#'
#' @param formula_cure Result of \code{parse_formula}
#' @param bg_model Background model index:
#'    1. exponential distribution
#'    2. fixed point values from life-tables
#'
#' @return list
#'
default_prior_cure <- function(formula_cure,
                               bg_model = 2) {

  nTx <- formula_cure$fe_nlevels[1]
  n_groups <- formula_cure$n_groups
  nvars <- formula_cure$nvars

  # treatment fixed effect
  mu_alpha_tx <- rep(-0.6, nTx)
  sigma_alpha_tx <- rep(0.8, nTx)

  # separate cure fractions empty parameters
  empty_alphas <- list(mu_alpha = numeric(0),
                       sigma_alpha = numeric(0))
  nempty <- length(empty_alphas)
  empty_alphas <- rep(empty_alphas, each = n_groups)

  # append group index
  names(empty_alphas) <- paste(names(empty_alphas), 1:nempty, sep = "_")

  # all cure fraction parameters
  empty_cf <- c(
    list(a_cf = numeric(0),
         b_cf = numeric(0),
         mu_alpha = numeric(0),
         sigma_alpha = numeric(0),
         mu_sd_cf = numeric(0),
         sigma_sd_cf = numeric(0)),
    empty_alphas)

  params_cf <-
    if (is_hier_cf(formula_cure)) {
      list(mu_sd_cf = rep(0, nTx),
           sigma_sd_cf = rep(2.5, nTx),
           mu_alpha = mu_alpha_tx,
           sigma_alpha = sigma_alpha_tx)
    } else if (is_separate_cf(formula_cure)) {
      list(mu_alpha    = matrix(rep(mu_alpha_tx, n_groups),
                                ncol = nTx, byrow = TRUE),
           sigma_alpha = matrix(rep(sigma_alpha_tx, n_groups),
                                ncol = nTx, byrow = TRUE))
    } else if (is_pooled_cf(formula_cure)) {
      list(a_cf = 1,
           b_cf = 1)
    }

  params_cf <-
    modifyList(empty_cf,
               params_cf)

  params_bg <-
    if (bg_model == 1) {
      list(mu_bg = c(-8.5, 0.03),
           sigma_bg = c(1, 1))
    } else {
      list(mu_bg = numeric(0),
           sigma_bg = numeric(0))}

  # latent submodel coefficients
  params_beta <-
    list(mu_0 = matrix(rep(0, nvars*n_groups),
                       nrow = n_groups),
         sigma_0 = matrix(rep(1, nvars*n_groups),
                          nrow = n_groups))

  c(params_cf,
    params_bg,
    params_beta)
}


