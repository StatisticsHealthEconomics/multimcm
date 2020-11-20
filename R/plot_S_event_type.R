
#' plot_S_event_type
#'
#' Plot results of running Stan
#' relative survival mixture cure model.
#'
#' @param file_names Nested list of file name for stan output
#'
#' @return ggplot object
#'
#' @importFrom purrr map
#' @importFrom reshape2 melt
#' @importFrom rstan extract
#' @importFrom dplyr mutate
#'
#' @examples
#' load("data/file_names.RData")
#'
plot_S_event_type <- function(file_names) {

  ##TODO:
  # add text cure fractions values

  fit_stan <- list()
  S_stats <- list()
  S_pred <- NULL

  event_types <- names(file_names)
  tx_names <- names(file_names[[1]])

  for (i in event_types) {

    fit_stan[[i]] <- list()
    S_stats[[i]] <- list()

    for (j in tx_names) {

      fit_stan[[i]][[j]] <-
        readRDS(file_names[[i]][[j]]) %>%
        rstan::extract()

      # rearrange to time as rows
      S_dat <-
        list(
          t(fit_stan[[i]][[j]]$S_pred) %>%
            as_tibble() %>%
            mutate(month = 1:n(),
                   type = "S_pred"),
          t(fit_stan[[i]][[j]]$S0) %>%
            as_tibble() %>%
            mutate(month = 1:n(),
                   type = "S0"),
          t(fit_stan[[i]][[j]]$S_bg) %>%
            as_tibble() %>%
            mutate(month = 1:n(),
                   type = "S_bg"))

      # means and credible intervals
      S_stats[[i]][[j]] <-
        S_dat %>%
        do.call(rbind, .) %>%
        melt(id.vars = c("month", "type")) %>%
        group_by(month, type) %>%
        summarise(mean = mean(value),
                  lower = quantile(value, probs = 0.025),
                  upper = quantile(value, probs = 0.975))
    }
  }

  # unnest
  plot_dat <-
    S_stats %>%
    map(bind_rows, .id = "Tx") %>%
    bind_rows(.id = "event_type") %>%
    mutate(scenario = paste(event_type, Tx, sep = "_"))

  ggplot(plot_dat, aes(month, mean, group = type, colour = type)) +
    geom_line() +
    # facet_grid(. ~ scenario)
    facet_grid(event_type ~ Tx) +
    ylab("Survival") +
    geom_ribbon(aes(x = month, ymin = lower, ymax = upper, fill = type),
                linetype = 0,
                alpha = 0.2)
}

