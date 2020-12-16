
#' prep_stan_params_all
#'
#' @param params User-defined values
#' @return hyper-parameters
#'
prep_stan_params_all <- function(params = NA) {

  ##TODO: add event_type argument and condition

  if (is.na(params)) params <- list(NA)

  modifyList(list(a_alpha = 1,       # gompertz
                  b_alpha = 1,
                  mu_0 = c(-3.1, 0), # exp
                  sigma_0 = c(1, 1)),
             params)
}

