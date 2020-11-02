
#' prep_stan_params
#'
prep_stan_params <- function(model) {

  if (model == "exp") {
    return(
      list(mu_0 = c(-3.1, -0.01),
           sigma_0 = c(1,1)))
  }
  if (model == "weibull") {
    return(
      list(a_alpha = 1,
           b_alpha = 1,
           mu_0 = c(-3.1, -0.01),
           sigma_0 = c(1,1)))
  }

  stop("distribution not found.")
}

