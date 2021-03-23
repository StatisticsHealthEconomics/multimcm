
#' plot_S_jointTx
#'
#' Plot results of running Stan
#' relative survival joint mixture cure model.
#'
#' @param stan_out
#' @param facet Two separate plots for os and pfs?
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
plot_S_jointTx <- function(stan_out = NA,
                           facet = TRUE,
                           annot_cf = FALSE) {

  S_stats <- list()

  stan_extract <- rstan::extract(stan_out)

  S_stats$os <-
    prep_S_dataTx(stan_extract,
                  event_type = "os")

  S_stats$pfs <-
    prep_S_dataTx(stan_extract,
                  event_type = "pfs")

  tx_names <- c("ipi", "nivo", "ipi+nivo")

  ann_text <-
    data.frame(
      event_type = c("os", "pfs"),
      Tx = rep(tx_names, each = 2),
      label = c(
        apply(X = stan_extract$cf_os, 2,
              FUN = function(x) paste(round(quantile(x, probs = c(0.05,0.5,0.95)), 2),
                                      collapse = " ")),
        apply(X = stan_extract$cf_pfs, 2,
              FUN = function(x) paste(round(quantile(x, probs = c(0.05,0.5,0.95)), 2),
                                      collapse = " "))))

  # unnest
  plot_dat <-
    S_stats %>%
    map(bind_rows, .id = "Tx") %>%
    bind_rows(.id = "event_type") %>%
    mutate(Tx = factor(Tx, levels = tx_names)) %>%
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
    p <-
      p + geom_text(data = ann_text,
                    aes(x = 40, y = 1, label = label),
                    inherit.aes = FALSE)}
  p
}

