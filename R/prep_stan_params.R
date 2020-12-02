
#' prep_stan_params
#'
#' @param model exp, weibull
#' @param params
#' @return hyper-parameters
#'
prep_stan_params <- function(model,
                             params = NA) {

  ##TODO: add event_type argument and condition

  if (!is.na(params)) return(params)

  if (model == "exp") {
    return(
      list(mu_0 = c(-3.1, 0),
           sigma_0 = c(1,1)))
  }
  if (model == "weibull") {
    return(
      list(a_alpha = 1,
           b_alpha = 1,
           mu_0 = c(-3.1, 0),
           sigma_0 = c(1,1)))
  }

  stop("distribution not found.")
}

