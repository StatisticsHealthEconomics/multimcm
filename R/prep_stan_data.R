
#' Prepare Stan data
#'
#' Data specific to end type for Stan input.
#'
#' @param formula_dat
#' @param event_type cluster/group
#' @param centre_vars Logical
#' @param bg_model Background model.
#'    1: Exponential distribution; 2: fixed point values from life-table
#' @param bg_hr background all-cause mortality hazard ratio
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
prep_stan_data <- function(formula_dat,
                           event_type,
                           centre_vars = FALSE,
                           bg_model = 1,
                           bg_hr = 1) {
  # one group only
  dat <-
    formula_dat$mf |>
    filter(!!sym(formula_dat$group_var) == event_type)

  # centre variables
  dat <-
    if (centre_vars) {
      dat |> mutate(
        across(where(~ is.numeric(.x) & !is.Surv(.x)),
               ~ round(.x - mean(.x))))
    }

  # drop treatment names
  fe_vars <- formula_dat$fe_vars[formula_dat$fe_vars != "TRTA"]

  X_mat <-
    as.data.frame(
      cbind(intercept = rep(1, nrow(dat)),
            dat[, fe_vars, drop = FALSE]))

  # X_mat <- model.matrix(~ TRTA, data = dat)

  # background hazard point values
  h_bg <- numeric(0)

  list(
    N = nrow(dat),              # total size
    n = array(table(dat$TRTA)), # group size by treatment
    t = dat[[1]][, "time"],
    d = dat[[1]][, "status"],   # censoring indicator
    H = ncol(X_mat),
    X = X_mat,
    h_bg = h_bg)
}

