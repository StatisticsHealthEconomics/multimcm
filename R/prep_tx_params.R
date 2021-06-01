
#' Treatment statistics from data
#'
#' Design matrix is simply diagonal matrix
#' at the moment but can include other covariates
#' in the future
#' i.e. a column of ones if for all treatments.
#'
#' @return list
#'
prep_tx_params <- function(input_data) {

  tx_names <- unique(input_data$TRTA)
  n_tx <- length(tx_names)
  tx_names <- factor(tx_names, levels = tx_names)

  Tx_dmat <- diag(n_tx)
  # model.matrix(~ tx_names + 0)

  c(Tx_dmat = list(Tx_dmat),
    nTx = n_tx)
}

