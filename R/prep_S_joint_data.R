
#' @importFrom glue glue
#'
prep_S_joint_data <- function(stan_out) {

  S_stats <- list()

  event_type <- 1:stan_out$n_endpoints
  model_names <- names(stan_out$models)

  stan_extract <- rstan::extract(stan_out)

  CI_probs <- c(0.025, 0.5, 0.975)

  S_stats <- list()

  for (i in 1:n_endpoints) {
    S_stats[[i]] <-
      prep_S_data(stan_extract,
                  event_type = i)

    label[i] <-
      apply(X = stan_extract[[glue("cf_{i}")]], 2,
            FUN = function(x)
              paste(round(quantile(x, probs = CI_probs), 2),
                    collapse = " "))
  }

  n_tx <- dim(stan_extract$cf_1)[2]

  ann_text <-
    data.frame(
      event_type = model_names,
      Tx = rep(1:n_tx, each = 2),
      label = label)

  # unnest
  plot_dat <-
    S_stats %>%
    map(bind_rows, .id = "Tx") %>%
    bind_rows(.id = "event_type") %>%
    mutate(scenario = paste(event_type, Tx, sep = "_"),
           type_tx = paste(type, Tx, sep = "_"),
           Tx = ifelse(type == "S_bg", "background",
                       ifelse(type == "^S_\\d$",
                              "uncured", Tx)),
           Tx = factor(Tx),
           endpoint = factor(toupper(event_type)),
           model_name = factor(model_name)) %>%
    arrange(endpoint)

  plot_dat
}
