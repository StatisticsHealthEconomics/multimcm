
#' Prepare Stan data in latent model
#'
#' Data specific to end type for Stan input.
#'
#' @param input_data
#' @param formula_cure parsed formula
#' @param formula_latent parsed formula
#' @param event_type cluster/group
#' @param centre_coefs Logical
#'    1: Exponential distribution; 2: fixed point values from life-table
#'
#' @return List consisting of:
#'         sample size,
#'         times,
#'         censoring indicator,
#'         number of covariates,
#'         covariates
#' @import dplyr
#' @export
#'
prep_latent_data <- function(input_data,
                             formula_cure,
                             formula_latent,
                             event_type,
                             centre_coefs = FALSE,
                             suffix = TRUE) {
  # one endpoint only
  dat <-
    input_data |>
    filter(!!sym(formula_cure$group_var) == event_type) |>
    arrange(!!sym(formula_cure$fe_vars[1]))

  # centre variables
  if (centre_coefs) {
    dat <-
      dat |> mutate(
        # across(where(~ is.numeric(.x) & !is.Surv(.x)), TODO:
        across(formula_latent$fe_vars,
               ~ round(.x - mean(.x))))
  }

  fe_vars <- formula_latent$fe_vars

  # design matrix
  X_mat <-
    as.data.frame(
      cbind(intercept = rep(1, nrow(dat)),
            dat[, fe_vars, drop = FALSE]))

  time_var <- toString(formula_latent$lhs$time)
  event_var <- toString(formula_latent$lhs$event)

  tx_name <- formula_cure$fe_vars[1]

  stan_data <- list(
    N = nrow(dat),                         # total size
    n = array(table(dat[[tx_name]])),      # group size by treatment
    t = unname(unlist(dat[, time_var])),
    d = unname(unlist(dat[, event_var])),  # censoring indicator
    H = ncol(X_mat),
    X = X_mat)

  # append unique id
  if (suffix && !identical(event_type, ""))
    names(stan_data) <- paste(names(stan_data), event_type, sep = "_")

  stan_data
}

