
#' Plot survival curves for joint model and all treatments
#'
#' Use results of running Stan with `bmcm_stan()`
#' relative survival joint mixture cure model.
#'
#' @description Generates a plot of the survival curves for the joint relative survival mixture cure model, 
#' overlaying the predicted survival probabilities and (optionally) the original Kaplan-Meier curves for all treatments.
#'
#' @param bmcm_out bmcm class output list, as returned by \code{bmcm_stan()}
#' @param facet Logical. Should the plots for each endpoint be separated into facets? Default is \code{TRUE}.
#' @param annot_cf Logical. Annotate the plot with the cure fractions? Default is \code{FALSE}.
#' @param add_km Logical. Include an overlaid Kaplan-Meier curve of the raw data? Default is \code{FALSE}.
#' @param add_marks Logical. Include Kaplan-Meier censoring marks? Default is \code{TRUE}.
#' @param ... Additional parameters passed to the plotting function.
#'
#' @return A \code{ggplot2} object showing the survival curves.
#'
#' @import survival
#' @importFrom purrr map
#' @importFrom reshape2 melt
#' @importFrom rstan extract
#' @importFrom dplyr mutate
#' @export
#'
#' @examples
#' \dontrun{
#' data("surv_input_data", package = "multimcm")
#' 
#' out <- bmcm_stan(
#'   input_data = surv_input_data,
#'   formula = "Surv(time=os, event=os_event) ~ 1",
#'   cureformula = "~ TRTA + (1 | center_id)",
#'   family_latent = "exponential",
#'   bg_model = "bg_fixed",
#'   bg_varname = "rate"
#' )
#' 
#' # Generate the survival plot, facetting by endpoint
#' surv_plot <- plot_S_joint(out, facet = TRUE, add_km = TRUE)
#' print(surv_plot)
#' }
plot_S_joint <- function(bmcm_out,
                         facet = TRUE,
                         annot_cf = FALSE,
                         add_km = FALSE,
                         add_marks = TRUE, ...) {

  plot_dat <- prep_S_joint_data(bmcm_out)

  add_facet <- function(facet) list(if (facet) facet_grid( ~ endpoint))

  p <-
    ggplot(plot_dat, aes(x = time, y = mean, group = type_tx, colour = Tx)) +
    geom_line() +
    add_facet(facet) +
    ylab("Survival") +
    ylim(0, 1) +
    geom_ribbon(aes(x = time, ymin = lower, ymax = upper, fill = Tx),
                linetype = 0,
                alpha = 0.2)

  # if (annot_cf) {
  #   p <-
  #     p + geom_text(data = ann_text,
  #                   aes(x = 40, y = 1, label = label),
  #                   inherit.aes = FALSE)}

  if (add_km) {
    km_curve <- geom_kaplan_meier(out_dat = bmcm_out)
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
    xlab("Time") +
    theme(text = element_text(size = 20))
}
