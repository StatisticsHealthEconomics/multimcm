
#' @params params_tx fixed effect Normal mus, sigmas
#'
prep_tx_params <- function(input_data,
                           params_tx = NA) {

  tx_names <- unique(input_data$TRTA)
  n_tx <- length(tx_names)
  tx_names <- factor(tx_names, levels = tx_names)

  cf_tx <-
    if (is.na(params_tx)) {
      list(mu_alpha = array(rep(-1, n_tx)),
           sigma_alpha = array(rep(1, n_tx)))
    } else {
      params_tx
    }

  Tx_dmat <- diag(n_tx)
    # model.matrix(~ tx_names + 0)

  c(Tx_dmat = list(Tx_dmat),
    nTx = n_tx,
    cf_tx)
}

