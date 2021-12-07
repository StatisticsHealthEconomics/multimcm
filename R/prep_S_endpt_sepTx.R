
#'
prep_S_endpt_sepTx <- function(stan_extract,
                               event_type = NA,
                               CI_probs = c(0.025, 0.5, 0.975)) {

  #
  if (is.na(event_type)) {
    S_pred <- "S_pred"
    S_0 <- "S_0"
  } else if (event_type == "os") {
    S_pred <- "S_os_pred"
    S_0 <- "S_os"
  } else {
    S_pred <- "S_pfs_pred"
    S_0 <- "S_pfs"
  }

  ##TODO; remove hardcoding
  n_tx <- 3
  S_stats <- vector("list", 3)

  for (i in seq_len(n_tx)) {

    # rearrange to time as rows
    S_dat <-
      list(
        t(stan_extract[[S_pred]][,,i]) %>%
          as_tibble() %>%
          rbind(1, .) %>%
          mutate(month = 0:(n() - 1),
                 type = S_pred),
        t(stan_extract[[S_0]][,,i]) %>%
          as_tibble() %>%
          rbind(1, .) %>%
          mutate(month = 0:(n() - 1),
                 type = S_0),
        t(stan_extract$S_bg[,,i]) %>%
          as_tibble() %>%
          rbind(1, .) %>%
          mutate(month = 0:(n() - 1),
                 type = "S_bg"))

    # means and credible intervals
    S_stats[[i]] <-
      S_dat %>%
      do.call(rbind, .) %>%
      melt(id.vars = c("month", "type")) %>%
      group_by(month, type) %>%
      summarise(mean = mean(value),
                lower = quantile(value, probs = CI_probs[1]),
                upper = quantile(value, probs = CI_probs[3]))
  }

  S_stats
}

