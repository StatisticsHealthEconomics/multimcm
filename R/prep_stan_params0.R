
#' prep_stan_params0
#'
#' @param model exp, weibull
#' @param params values to over-write defaults
#' @return hyper-parameters
#'
prep_stan_params0 <- function(model,
                              params = NA) {

  ##TODO: add event_type argument and condition

  if (is.na(params)) params <- list(NA)

  if (model == "exp") {
    return(
      modifyList(
        list(a_alpha = numeric(0),
             b_alpha = numeric(0),
             mu_0 = c(-3.1, 0),
             sigma_0 = c(1, 1)),
        params))
  }
  if (model == "weibull") {
    return(
      modifyList(
        list(a_alpha = 1,
             b_alpha = 1,
             mu_0 = c(-3.1, 0),
             sigma_0 = c(1, 1)),
        params))
  }

  stop("distribution not found.")
}

