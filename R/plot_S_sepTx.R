
#' Plot survival curves for separate models and all treatments
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
#' stan_out <- list (
#' readRDS("~/R/rstanbmcm/vignettes/checkmate_report_data/bg_fixed/stan_exp_exp_IPILIMUMAB.Rds"),
#' readRDS("~/R/rstanbmcm/vignettes/checkmate_report_data/bg_fixed/stan_exp_exp_NIVOLUMAB.Rds"),
#' readRDS("~/R/rstanbmcm/vignettes/checkmate_report_data/bg_fixed/stan_exp_exp_NIVOLUMAB+IPILIMUMAB.Rds"))
#'
plot_S_sepTx <- function(stan_out = NA,
                         facet = TRUE,
                         annot_cf = FALSE,
                         data = NA) {

  plot_dat <- prep_S_sepTx_data(stan_out)

  plot_dat$`Uncured only` <- plot_dat$type %in% c("S_os", "S_pfs")

  # get names direct from Stan fit
  model_names <- strsplit(stan_out[[1]]@model_name, "_")[[1]][c(1, 2)]

  add_facet <- function(facet) {list(if (facet) facet_grid( ~ event_type))}

  p <-
    ggplot(plot_dat, aes(x = month, y = mean, group = type_tx, colour = Tx)) +
    geom_line(aes(linetype = `Uncured only`), size = 1.2) +
    add_facet(facet) +
    ylab("Survival") +
    geom_ribbon(aes(x = month, ymin = lower, ymax = upper, fill = Tx),
                linetype = 0,
                alpha = 0.15) +
    ylim(0, 1)

  if (annot_cf) {
    p <-
      p + geom_text(data = ann_text,
                    aes(x = 40, y = 1, label = label),
                    inherit.aes = FALSE)}

  if (!any(is.na(data))) {
    km_curve <- geom_kaplan_meier(data = data,
                                  event_type = model_names)
  } else {
    km_curve <- NULL}

  curve_cols <- c("turquoise1", "blue", "cyan4", "green")
  curve_labs <- c("Ipilimumab", "Nivolumab", "Nivolumab & Ipilimumab", "Background", "Uncured")

  p +
    km_curve +
    xlim(0, 60) +
    scale_fill_manual(labels = curve_labs,
                      values = curve_cols) +
    scale_color_manual(labels = curve_labs,
                       values = curve_cols) +
    scale_linetype_manual(values=c("solid", "dashed")) +
    guides(color = guide_legend(""), fill = guide_legend("")) +
    theme_bw() +
    xlab("Month") +
    theme(text = element_text(size = 10))
}
