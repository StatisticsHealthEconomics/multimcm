
#' Plot survival curves for joint model and all treatments
#'
#' Use results of running Stan with \code{bmcm_stan()}
#' relative survival joint mixture cure model.
#'
#' @param out bmcm class output list
#' @param facet Two separate plots for each end point or overlaid?
#' @param annot_cf Annotate with cure fractions? Logical
#' @param add_km Include Kaplan-Meier layer? Logical
#' @param ... Additional parameters
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
plot_S_joint <- function(bmcm_out,
                         facet = TRUE,
                         annot_cf = FALSE,
                         add_km = FALSE, ...) {

  plot_dat <- prep_S_joint_data(bmcm_out)

  add_facet <- function(facet) list(if (facet) facet_grid( ~ endpoint))

  p <-
    ggplot(plot_dat, aes(x = month, y = mean, group = type_tx, colour = Tx)) +
    geom_line() +
    add_facet(facet) +
    ylab("Survival") +
    ylim(0, 1) +
    geom_ribbon(aes(x = month, ymin = lower, ymax = upper, fill = Tx),
                linetype = 0,
                alpha = 0.2)

  if (annot_cf) {
    p <-
      p + geom_text(data = ann_text,
                    aes(x = 40, y = 1, label = label),
                    inherit.aes = FALSE)}

  if (add_km) {
    km_curve <- geom_kaplan_meier(data = bmcm_out)
  } else {
    km_curve <- NULL}

  ##TODO:
  # curve_cols <-
  # curve_labs <- c("Background", "Uncured")

  p +
    km_curve +
    xlim(0, 60) +
    # scale_fill_manual(labels = curve_labs,
    #                   values = curve_cols) +
    # scale_color_manual(labels = curve_labs,
    #                    values = curve_cols) +
    guides(color = guide_legend(""),
           fill  = guide_legend("")) +
    theme_bw() +
    xlab("Month") +
    theme(text = element_text(size = 20))
}
