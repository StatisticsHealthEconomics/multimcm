
#' Plot survival curves for joint model and all treatments
#'
#' Use results of running Stan
#' relative survival joint mixture cure model.
#'
#' @param stan_out Stan output data frame
#' @param facet Two separate plots for OS and PFS or overlaid?
#' @param annot_cf Annotate with cure fractions? Logical
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

  n_tx <- dim(stan_extract$alpha)[2]

  ann_text <-
    data.frame(
      event_type = c("os", "pfs"),
      Tx = rep(1:n_tx, each = 2),
      label = c(
        apply(X = stan_extract$cf_os, 2,
              FUN = function(x)
                paste(round(quantile(x, probs = c(0.05,0.5,0.95)), 2),
                      collapse = " ")),
        apply(X = stan_extract$cf_pfs, 2,
              FUN = function(x)
                paste(round(quantile(x, probs = c(0.05,0.5,0.95)), 2),
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
           Tx = factor(Tx))

  add_facet <- function(facet) {list(if (facet) facet_grid( ~ event_type))}

  p <-
    ggplot(plot_dat, aes(month, mean, group = type_tx, colour = Tx)) +
    # ggplot(plot_dat, aes(month, mean, group = type, colour = type)) +
    geom_line() +
    add_facet(facet) +
    ylab("Survival") +
    geom_ribbon(aes(x = month, ymin = lower, ymax = upper, fill = Tx),
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

