
#
prep_S_joint_data <- function(stan_out) {

  S_stats <- list()
  model_names <-
    gsub("_", " ", strsplit(stan_out@model_name, " ")[[1]])

  stan_extract <- rstan::extract(stan_out)

  CI_probs <- c(0.025, 0.5, 0.975)

  S_stats$os <-
    prep_S_dataTx(stan_extract,
                  event_type = "os")

  S_stats$pfs <-
    prep_S_dataTx(stan_extract,
                  event_type = "pfs")

  n_tx <- dim(stan_extract$cf_os)[2]

  ann_text <-
    data.frame(
      event_type = model_names,
      # event_type = c("os", "pfs"),
      Tx = rep(1:n_tx, each = 2),
      label = c(
        apply(X = stan_extract$cf_os, 2,
              FUN = function(x)
                paste(round(quantile(x, probs = CI_probs), 2),
                      collapse = " ")),
        apply(X = stan_extract$cf_pfs, 2,
              FUN = function(x)
                paste(round(quantile(x, probs = CI_probs), 2),
                      collapse = " "))))
  # unnest
  plot_dat <-
    S_stats %>%
    map(bind_rows, .id = "Tx") %>%
    bind_rows(.id = "event_type") %>%
    mutate(scenario = paste(event_type, Tx, sep = "_"),
           type_tx = paste(type, Tx, sep = "_"),
           Tx = ifelse(type == "S_bg", "background",
                       ifelse(type == "S_os" | type == "S_pfs",
                              "uncured", Tx)),
           Tx = factor(Tx),
           endpoint = factor(toupper(event_type), c("PFS", "OS")),
           event_type = ifelse(event_type == "os",
                               model_names[1], model_names[2])) %>%
    arrange(endpoint)

  plot_dat
}
