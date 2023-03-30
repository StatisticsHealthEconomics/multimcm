
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
  mu_alpha_fe <- rep(-1, nTx)
  sigma_alpha_fe <- rep(0.5, nTx)

  if (is_separate_cf(formula_cure)) {
    empty_alphas <- NULL
  } else {
    # separate cure fractions empty parameters
    empty_alphas <- list(mu_alpha = numeric(0),
                         sigma_alpha = numeric(0))
    empty_alphas <- rep(empty_alphas, each = n_groups)

    # append group index
    names(empty_alphas) <- paste(names(empty_alphas), 1:n_groups, sep = "_")
  }

  # all cure fraction parameters
  empty_cf <- c(
    list(a_cf = numeric(0),
         b_cf = numeric(0),
         mu_alpha = numeric(0),
         sigma_alpha = numeric(0),
         mu_sd_cf = numeric(0),
         sigma_sd_cf = numeric(0)),
    empty_alphas)

  if (is_hier_cf(formula_cure)) {
    params_cf <-
      list(
        mu_sd_cf = rep(0, nTx),
        # sigma_sd_cf = rep(2.5, nTx),     # half-normal
        sigma_sd_cf = rep(25/10, nTx),      # half-cauchy
        #  sigma_sd_cf = rep(2.5, nTx),    # half-t
        # min_sd_cf = rep(0.001, nTx),     # uniform
        # max_sd_cf = rep(4, nTx),          # uniform
        # mu_sd_cf = rep(0.2, nTx),   # narrow
        # lambda_sd_cf = rep(46, nTx),
        mu_alpha = mu_alpha_fe,
        sigma_alpha = sigma_alpha_fe)
  } else if (is_separate_cf(formula_cure)) {
    params_cf <-
      list(mu_alpha = mu_alpha_fe,
           sigma_alpha = sigma_alpha_fe) |>
      rep(formula_cure$n_groups)

    names(params_cf) <-
      paste0(names(params_cf), "_", rep(1:2, each = formula_cure$n_groups))
  } else if (is_pooled_cf(formula_cure)) {
    params_cf <-
      list(a_cf = 3,
           b_cf = 8)
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

  c(params_cf, params_bg)
}


