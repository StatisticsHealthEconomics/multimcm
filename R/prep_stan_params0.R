
#' prep_stan_params0
#'
#' Create a list of all parameters and those not
#' included in the distribution are set to empty.
#'
#' @param model exp, weibull
#' @param params values to over-write defaults
#' @return hyper-parameters
#'
prep_stan_params0 <- function(model,
                              params = NA) {

  ##TODO: add event_type argument and condition

  if (is.na(params)) params <- list(NA)

  empty_params <-
    list(a_alpha = numeric(0),
         b_alpha = numeric(0))

  if (model == "exp") {
    return(
      modifyList(
        empty_params,
        modifyList(
          list(mu_0 = c(-3.1, 0),
               sigma_0 = c(1, 1)),
          params)))
  }
  if (model == "weibull") {
    return(
      modifyList(
        empty_params,
        modifyList(
          list(a_alpha = 1,
               b_alpha = 1,
               mu_0 = c(-3.1, 0),
               sigma_0 = c(1, 1)),
          params)))
  }

  stop("distribution not found.")
}

