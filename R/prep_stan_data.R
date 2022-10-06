
#' Prepare Stan data
#'
#' Data specific to OS or PFS for Stan input.
#'
#' @param input_data Survival individual level data
#' @param event_type cluster/group
#' @param centre_age Logical
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
prep_stan_data <- function(input_data,
                           event_type,
                           centre_age,
                           bg_model = 1,
                           bg_hr = 1) {

  input_data <- arrange(input_data, TRTA)

  tx_dat <-
    input_data |>
    filter(event == event_type)
    select(TRTA, status, month, age_event, rate) |>
    mutate(rate = ifelse(rate == 0, 0.00001, rate)) # replace so >0

  # centering
  age_adj <- ifelse(centre_age, mean(tx_dat[[4]]), 0)

  X_age <-
    as.data.frame(
      matrix(c(rep(1, nrow(tx_dat)),
               tx_dat[[4]] - age_adj),
             byrow = FALSE,
             ncol = 2))

  # X_tx <- model.matrix(~ TRTA, data = tx_dat)

  # background hazard point values
  h_bg <-
    if (bg_model == 2) {
      tx_dat[[5]]*bg_hr
    } else {
      numeric(0)}

  ## hazard ratio
  # dmat <- model.matrix(~ TRTA, data = tx_dat)[, -1]

  list(
    N = nrow(tx_dat),
    n = array(table(input_data$TRTA)),
    t = tx_dat[[2]],
    d = tx_dat[[3]],
    H = 2,
    X = X_age,
    h_bg = h_bg)
}

