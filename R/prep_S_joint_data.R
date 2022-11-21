
#' @importFrom glue glue
#'
prep_S_joint_data <- function(bmcm_out) {

  S_stats <- list()
  n_groups <- bmcm_out$formula$cure$n_groups

  event_type <- 1:n_groups
  model_names <-bmcm_out$distns
  n_tx <- bmcm_out$formula$cure$cf_idx

  stan_extract <- rstan::extract(bmcm_out$output)

  browser()

  CI_probs <- c(0.025, 0.5, 0.975)

  S_stats <- list()

  # summary statistics for each end point
  for (i in seq_len(n_groups)) {

    S_stats[[i]] <-
      prep_S_data(stan_extract,
                  event_type = i)

    ##TODO:
    # label[[i]] <-
    #   apply(X = stan_extract[[glue("cf_{i}")]], 2,
    #         FUN = function(x)
    #           paste(round(quantile(x, probs = CI_probs), 2),
    #                 collapse = " "))
  }

  ann_text <-
    data.frame(
      event_type = model_names,
      Tx = rep(1:n_tx, each = 2))#,
  # label = label)

  # unnest
  plot_dat <-
    S_stats %>%
    map(bind_rows, .id = "Tx") %>%
    bind_rows(.id = "event_type") %>%
    mutate(scenario = paste(event_type, Tx, sep = "_"),
           type_tx = paste(type, Tx, sep = "_"),
           Tx = ifelse(type == "S_bg", "background",
                       ifelse(grepl("^S_\\d$", type),
                              "uncured", Tx)),
           Tx = factor(Tx),
           endpoint = factor(toupper(event_type))
           # model_name = as.factor(model_names)  ##TODO:
    ) %>%
    arrange(endpoint)

  plot_dat
}
