
#' plot_S_event_type
#'
#' Plot results of running Stan
#' relative survival mixture cure model.
#'
#' @param file_names Nested list of file names for Stan output
#' @param stan_list List of Stan output
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
plot_S_event_type <- function(file_names = NA,
                              stan_list = NA) {

  ##TODO:
  # add text cure fractions values

  fit_stan <- list()
  S_stats <- list()
  S_pred <- NULL

  # read-in output or use directly
  if (!all(is.na(file_names))) {
    stan_out <- function(i, j) readRDS(file_names[[i]][[j]])
    event_types <- names(file_names)
    tx_names <- names(file_names[[1]])
  } else {
    stan_out <- function(i, j) stan_list[[i]][[j]]
    event_types <- names(stan_list)
    tx_names <- names(stan_list[[1]])}

  for (i in event_types) {

    fit_stan[[i]] <- list()
    S_stats[[i]] <- list()

    for (j in tx_names) {

      fit_stan[[i]][[j]] <-
        stan_out(i, j) %>%
        rstan::extract()

      S_stats[[i]][[j]] <-
        prep_S_data(fit_stan[[i]][[j]])
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

