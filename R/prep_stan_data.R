
#' Prepare Stan data
#'
#' Data specific to end type for Stan input.
#'
#' @param formula_cure
#' @param formula_latent
#' @param event_type cluster/group
#' @param centre_coefs Logical
#'    1: Exponential distribution; 2: fixed point values from life-table
#'
#' @return List;
#'         sample size,
#'         times,
#'         censoring indicator,
#'         number of covariates,
#'         covariates
#' @import dplyr
#' @export
#'
prep_stan_data <- function(formula_cure,
                           formula_latent,
                           event_type,
                           centre_coefs = FALSE,
                           suffix = TRUE) {
  # one group only
  dat <-
    ##TODO: merge so don't repeat covariates in both
    ##      are they in the same order? use ids
    cbind(formula_latent$mf, formula_cure$mf) |>
    filter(!!sym(formula_cure$group_var) == event_type)

  # centre variables
  dat <-
    if (centre_coefs) {
      dat |> mutate(
        across(where(~ is.numeric(.x) & !is.Surv(.x)),
               ~ round(.x - mean(.x))))
    }

  # drop treatment names
  fe_vars <- formula_latent$fe_vars

  # design matrix
  X_mat <-
    as.data.frame(
      cbind(intercept = rep(1, nrow(dat)),
            dat[, fe_vars, drop = FALSE]))

  stan_data <- list(
    N = nrow(dat),              # total size
    n = array(table(dat$TRTA)), # group size by treatment
    t = dat[[1]][, "time"],
    d = dat[[1]][, "status"],   # censoring indicator
    H = ncol(X_mat),
    X = X_mat,
    h_bg = numeric(0))          # background hazard point values

  # append unique id
  if (suffix && !identical(event_type, ""))
    names(stan_data) <- paste(names(stan_data), event_type, sep = "_")

  stan_data
}

