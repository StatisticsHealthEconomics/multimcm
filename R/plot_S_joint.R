
#' plot_S_joint
#'
#' Plot results of running Stan
#' relative survival joint mixture cure model.
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
plot_S_joint <- function(file_names = NA,
                         stan_list = NA) {

  fit_stan <- list()
  S_stats <- list()
  S_pred <- NULL

  # read-in output or use directly
  if (!all(is.na(file_names))) {
    stan_out <- function(j) readRDS(file_names[[j]])
    tx_names <- names(file_names[[1]])
  } else {
    stan_out <- function(j) stan_list[[j]]
    tx_names <- names(stan_list[[1]])}

  fit_stan <- list()
  S_stats <- list()

  for (j in tx_names) {

    fit_stan[[j]] <-
      stan_out(j) %>%
      rstan::extract()

    S_stats[[1]][[j]] <-
      prep_S_data(fit_stan[[j]],
                  event_type = "os")

    S_stats[[2]][[j]] <-
      prep_S_data(fit_stan[[j]],
                  event_type = "pfs")
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

