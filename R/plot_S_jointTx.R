
#' Plot survival curves for joint model and all treatments
#'
#' Use results of running Stan
#' relative survival joint mixture cure model.
#'
#' @param stan_out Stan output data frame
#' @param facet Two separate plots for OS and PFS or overlaid?
#' @param annot_cf Annotate with cure fractions? Logical
#' @param data Study individual-level data for Kaplan-Meier
#'
#' @return ggplot object
#'
#' @import survival
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
                           annot_cf = FALSE,
                           data = NA) {

  plot_dat <- prep_S_jointTx_data(stan_out)

  add_facet <- function(facet) {list(if (facet) facet_grid( ~ event_type))}

  p <-
    ggplot(plot_dat, aes(x = month, y = mean, group = type_tx, colour = Tx)) +
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

  if (!any(is.na(data))) {
    km_curve <- geom_kaplan_meier(data = data)
  } else {
    km_curve <- NULL}

  curve_cols <- c("turquoise1", "blue", "cyan4", "green", "tomato1")
  curve_labs <- c("Ipilimumab", "Nivolumab", "Nivolumab & Ipilimumab", "Background", "Uncured")

  p +
    km_curve +
    xlim(0, 60) +
    scale_fill_manual(labels = curve_labs,
                      values = curve_cols) +
    scale_color_manual(labels = curve_labs,
                       values = curve_cols) +
    guides(color = guide_legend(""), fill = guide_legend("")) +
    theme_bw() +
    xlab("Month") +
    theme(text = element_text(size = 10))
}

