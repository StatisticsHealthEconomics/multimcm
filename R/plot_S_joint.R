
#' plot_S_joint
#'
#' Plot results of running Stan
#' relative survival joint mixture cure model.
#'
#' @param file_names Nested list of file names for Stan output
#' @param stan_list List of Stan output
#' @param facet Two separate plots for os and pfs or overlaid?
#' @param annot_cf Annotate with cure fractions?
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
                         stan_list = NA,
                         facet = TRUE,
                         annot_cf = TRUE) {

  fit_stan <- list()
  S_stats <- list()
  S_pred <- NULL

  # read-in output or use directly
  if (!all(is.na(file_names))) {
    stan_out <- function(j) readRDS(file_names[[j]])
    tx_names <- names(file_names[[1]])
  } else {
    stan_out <- function(j) stan_list[[j]]
    tx_names <- names(stan_list)}

  fit_stan <- list()
  S_stats <- list()
  S_stats$os <- list()
  S_stats$pfs <- list()

  for (j in tx_names) {

    fit_stan[[j]] <-
      stan_out(j) %>%
      rstan::extract()

    S_stats$os[[j]] <-
      prep_S_data(fit_stan[[j]],
                  event_type = "os")

    S_stats$pfs[[j]] <-
      prep_S_data(fit_stan[[j]],
                  event_type = "pfs")
  }

  ann_text <-
    data.frame(event_type = c("os", "pfs"),
               Tx = rep(tx_names, each = 2),
               label = c(paste(round(
                 quantile(fit_stan[[1]]$cf_os,
                          probs = c(0.05,0.5,0.95)), 2), collapse = " "),
                 paste(round(
                   quantile(fit_stan[[1]]$cf_pfs,
                            probs = c(0.05,0.5,0.95)), 2), collapse = " ")))
  # unnest
  plot_dat <-
    S_stats %>%
    map(bind_rows, .id = "Tx") %>%
    bind_rows(.id = "event_type") %>%
    mutate(scenario = paste(event_type, Tx, sep = "_"))

  add_facet <- function(facet) {list(if (facet) facet_grid(Tx ~ event_type))}

  p <-
    ggplot(plot_dat, aes(month, mean, group = type, colour = type)) +
    geom_line() +
    add_facet(facet) +
    ylab("Survival") +
    geom_ribbon(aes(x = month, ymin = lower, ymax = upper, fill = type),
                linetype = 0,
                alpha = 0.2) +
    ylim(0, 1)

  if (annot_cf) {
    p <- p + geom_text(data = ann_text,
                       aes(x = 40, y = 1, label = label), inherit.aes = FALSE)}
  p
}

