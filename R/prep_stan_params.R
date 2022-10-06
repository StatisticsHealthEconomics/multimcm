
#' Prep Stan parameters
#'
#' Default parameters for priors if not supplied.
#'
#' @param model exp, loglogistic, weibull, gompertz, lognormal. gengamma
#' @param params user-supplied hyperparameters
#' @return hyper-parameters
#' @name prep_stan_params
NULL


#' @rdname prep_stan_params
prep_distn_params <- function(model,
                              params = NA) {

  if (!any(is.na(params))) return(params)

  switch(model,
         exp =
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
}



