
#' default_prior_latent
#'
#' Default parameters for survival model priors if not supplied.
#'
#' @return hyper-parameters
#' @name prep_stan_params
NULL


#' @rdname prep_stan_params
#'
default_prior_latent <- function(formula_latent,
                                 formula_cure) {

  model <- formula_latent$family
  n_group <- formula_cure$n_group
  nvars <- formula_latent$fe_nvars

  model_params <-
    switch(model,
           exp =
             list(mu_S = c(-3, rep(0, nvars)),
                  sigma_S = c(0.5, rep(0.01, nvars))),
           loglogistic =
             list(a_shape = 1,
                  b_shape = 1,
                  mu_S = c(3, rep(0, nvars)),
                  sigma_S = c(0.5, rep(0.01, nvars))),
           weibull =
             list(a_shape = 1,
                  b_shape = 1,
                  mu_S = c(3, rep(0, nvars)),
                  sigma_S = c(0.5, rep(0.01, nvars))),
           gompertz =
             list(a_shape = 1,
                  b_shape = 1000,
                  mu_S = c(-3, rep(0, nvars)),
                  sigma_S = c(0.5, rep(0.01, nvars))),
           lognormal =
             list(a_sd = 1,
                  b_sd = 2,
                  mu_S = c(1.5, rep(0, nvars)),
                  sigma_S = c(0.5, rep(0.01, nvars))),
           gengamma =
             list(a_mu = 1,
                  b_mu = 1,
                  a_Q = 2,
                  b_Q = 1,
                  mu_S = c(-3, rep(0, nvars)),
                  sigma_S = c(0.5, rep(0.01, nvars))),
           stop("distribution not found."))

  # ensure consistent dimensions
  if (nvars == 0) {
    model_params$mu_S <- array(model_params$mu_S, 1)
    model_params$sigma_S <- array(model_params$sigma_S, 1)
  }

  # same parameters for each cluster
  n_params <- length(model_params)
  new_names <- paste(names(model_params),
                     rep(1:n_group, each = n_params), sep = "_")
  rep(model_params, n_group) |> setNames(new_names)
}



