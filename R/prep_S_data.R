
#' Prepare posterior survival data for plotting
#'
prep_S_data <- function(stan_extract,
                        event_type = NA,
                        CI_probs = c(0.025, 0.5, 0.975),
                        tx_id = NA) {
  # name curves by event type
  if (is.na(event_type)) {
    S_pred <- "S_pred"
    S_0 <- "S_0"
  } else {
    S_pred <- glue::glue("S_{event_type}_pred")
    S_0 <- glue::glue("S_{event_type}")
  }

  if (is.na(tx_idx))
    tx_idx <- seq_len(dim(stan_extract$cf_1)[2])

  S_stats <- list()

  for (i in tx_idx) {

    # rearrange to time as rows
    S_dat <-
      list(
        t(stan_extract[[S_pred]][,,i]) %>%
          as_tibble() %>%
          rbind(1, .) %>%
          mutate(time = 0:(n() - 1),
                 type = S_pred),
        t(stan_extract[[S_0]]) %>%
          as_tibble() %>%
          rbind(1, .) %>%
          mutate(time = 0:(n() - 1),
                 type = S_0),
        t(stan_extract$S_bg) %>%
          as_tibble() %>%
          rbind(1, .) %>%
          mutate(time = 0:(n() - 1),
                 type = "S_bg"))

    # means and credible intervals
    S_stats[[i]] <-
      S_dat %>%
      do.call(rbind, .) %>%
      melt(id.vars = c("time", "type")) %>%
      group_by(time, type)  |>
      summarise(mean = mean(value),
                lower = quantile(value, probs = CI_probs[1]),
                upper = quantile(value, probs = CI_probs[3])) |>
      mutate(Tx = i)
  }

  S_stats
}

