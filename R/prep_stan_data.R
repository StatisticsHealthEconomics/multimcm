
#' Prepare Stan data
#'
#' Data specific to end type for Stan input.
#'
#' @param dat
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
prep_stan_data <- function(dat,
                           event_type,
                           centre_vars = FALSE,
                           bg_model = 1,
                           bg_hr = 1) {
  tx_dat <-
    dat$mf |>
    filter(!!sym(dat$group_var) == event_type)

  # centre
  tx_dat <-
    if (centre_vars) {
      tx_dat |> mutate(
        across(c(where(is.numeric), -(1:2)), ~ round(.x - mean(.x))))
    }

  X_mat <-
    as.data.frame(
      cbind(intercept = rep(1, nrow(tx_dat)),
            tx_dat[, -(1:3), drop = FALSE]))

  # X_tx <- model.matrix(~ TRTA, data = tx_dat)

  # background hazard point values
  h_bg <- numeric(0)

  list(
    N = nrow(tx_dat),
    n = array(table(tx_dat$TRTA)),
    t = tx_dat[[1]][, "time"],
    d = tx_dat[[1]][, "status"],
    H = ncol(X_mat),
    X = X_mat,
    h_bg = h_bg)
}

