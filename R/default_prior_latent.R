
#' Default prior latent variable
#'
#' Default parameters for survival model priors if not supplied.
#'
#' @return hyper-parameters
#' @name prep_stan_params
NULL


#' @rdname prep_stan_params
#' @importFrom purrr flatten
#'
default_prior_latent <- function(formula_latent,
                                 formula_cure) {

  model <- formula_latent$family
  n_models <- length(model)
  n_group <- formula_cure$re_nlevels[1]
  nvars <- formula_latent$fe_nvars

  mod_pars <- pars_format(nvars)

  model_params <-
    lapply(model, \(x)
           switch(x,
                  exp =
                    list(mu_S = mod_pars(-3, 0),
                         sigma_S = mod_pars(0.5, 0.01)),
                  loglogistic =
                    list(a_shape = 1,
                         b_shape = 1,
                         mu_S = mod_pars(-3, 0),
                         sigma_S = mod_pars(0.5, 0.01)),
                  weibull =
                    list(a_shape = 1,
                         b_shape = 1,
                         mu_S = mod_pars(-3, 0),
                         sigma_S = mod_pars(0.5, 0.01)),
                  gompertz =
                    list(a_shape = 1,
                         b_shape = 1000,
                         mu_S = mod_pars(-3, 0),
                         sigma_S = mod_pars(0.5, 0.01)),
                  lognormal =
                    list(a_sd = 1,
                         b_sd = 2,
                         mu_S = mod_pars(1.5, 0),
                         sigma_S = mod_pars(0.5, 0.01)),
                  gengamma =
                    list(a_scale = log(0.9),
                         b_scale = 0.1,
                         a_Q = -0.7,
                         b_Q = 0.6,
                         mu_S = mod_pars(0.3, 0),
                         sigma_S = mod_pars(0.6, 0.1)),
                  stop("distribution not found.")))

  # label index within each cluster
  model_params <-
    lapply(1:n_models, \(x)
           setNames(model_params[[x]],
                    paste(names(model_params[[x]]), x, sep = "_")))

  purrr::flatten(model_params)
}


#' Parse variables
#'
#' Ensure consistent dimensions
#' depending on number of covariates
#' @keywords internal
#'
pars_format <- function(nvars) {
  force(nvars)
  function(x, y=NA) {
    if (nvars == 0) {
      array(x, 1)
    } else {
      c(x, rep(y, nvars))
    }}
}

