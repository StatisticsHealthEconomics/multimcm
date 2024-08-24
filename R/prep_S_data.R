
#' Prepare posterior survival data for plotting
#'
prep_S_data <- function(stan_extract,
                        event_type = NA,
                        CI_probs = c(0.025, 0.5, 0.975),
                        tx_idx = NA) {
  # name curves by event type
  if (is.na(event_type)) {
    S_pred <- "S_pred"
    S_0 <- "S_0"
  } else {
    S_pred <- glue::glue("S_{event_type}_pred")
    S_0 <- glue::glue("S_{event_type}")
  }

  # create treatment indices
  if (is.na(tx_idx)) {
    ntx <- dim(stan_extract$cf_1)[2]
    tx_idx <- seq_len(ntx)
  }

  S_stats <- list()

  for (i in tx_idx) {

    # extract survival data

    S_0_extract <- stan_extract[[S_0]]
    S_bg_extract <- stan_extract$S_bg

    S_pred_data <- stan_extract[[S_pred]]

    if (length(dim(S_pred_data)) == 2) {
      S_pred_extract <-
        S_pred_data[, grep(names(S_pred_data), pattern = glue::glue("{i}\\]$"))]
    } else {
      S_pred_extract <- S_pred_data[,,i]
    }

    # rearrange to time as rows
    S_dat <-
      list(
        t(S_pred_extract) |>
          as_tibble() |>
          rbind(1, ... = _) |>
          mutate(time = 0:(n() - 1),
                 type = S_pred),
        t(S_0_extract) |>
          as_tibble() |>
          rbind(1, ... = _) |>
          mutate(time = 0:(n() - 1),
                 type = S_0),
        t(S_bg_extract) |>
          as_tibble() |>
          rbind(1, ... = _) |>
          mutate(time = 0:(n() - 1),
                 type = "S_bg"))

    # means and credible intervals
    S_stats[[i]] <-
      S_dat |>
      do.call(rbind, args = _) |>
      melt(id.vars = c("time", "type")) |>
      group_by(time, type)  |>
      summarise(mean = mean(value),
                lower = quantile(value, probs = CI_probs[1]),
                upper = quantile(value, probs = CI_probs[3])) |>
      mutate(Tx = i)
  }

  S_stats
}

