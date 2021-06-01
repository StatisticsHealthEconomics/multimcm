
#' Treatment statistics from data
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

