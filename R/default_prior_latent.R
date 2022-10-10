
#' default_prior_latent
#'
#' Default parameters for priors if not supplied.
#'
#' @return hyper-parameters
#' @name prep_stan_params
NULL


#' @rdname prep_stan_params
default_prior_latent <- function(formula_latent) {

  model <- formula_latent$family
  n_group <- formula_latent$n_group

  switch(model,
         exponential =
           list(mu_0 = c(-3, 0),
                sigma_0 = c(0.5, 0.01)),
         loglogistic =
           list(a_shape = 1,
                b_shape = 1,
                mu_0 = c(3, 0),
                sigma_0 = c(0.5, 0.01)),
         weibull =
           list(a_shape = 1,
                b_shape = 1,
                mu_0 = c(3, 0),
                sigma_0 = c(0.5, 0.01)),
         gompertz =
           list(a_shape = 1,
                b_shape = 1000,
                mu_0 = c(-3, 0),
                sigma_0 = c(0.5, 0.01)),
         lognormal =
           list(a_sd = 1,
                b_sd = 2,
                mu_0 = c(1.5, 0),
                sigma_0 = c(0.5, 0.01)),
         gengamma =
           list(a_mu = 1,
                b_mu = 1,
                a_Q = 2,
                b_Q = 1,
                mu_0 = c(-3, 0),
                sigma_0 = c(0.5, 0.01)),
         stop("distribution not found."))

  # same parameters for each cluster
  for (i in seq_len(n_groups)) {
    res[[i]] <- model
  }

  res
}



