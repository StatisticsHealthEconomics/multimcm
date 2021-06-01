
#' Cure fraction forest plot using Stan output
#'
#' Both OS and PFS for single treatment.
#' Using single treatment Stan model.
#'
#' @param folder string
#' @param trt treatment name string
#' @param is_hier Is hierarchical model (or separate)?
#'
#' @importFrom epicontacts, adegenet
#' @import dplyr ggplot2 tidybayes purrr
#'
#' @return
#' @export
#'
cf_forest_plot <- function(folder = "data/independent/cf hier/bg_fixed_hr1/",
                           trt = "NIVOLUMAB",
                           is_hier = TRUE) {

  stan_summary <- function(stan_obj) {
    summary(stan_obj,
            par = c("cf_global", "cf_os", "cf_pfs"),
            probs = c(0.25, 0.75))$summary}

  filenames <-
    list.files(folder,
               pattern = gsub("\\+", "\\\\+", paste0("_", trt, ".Rds$")),
               full.names = TRUE)

  stan_list <- map(filenames, readRDS)
  names(stan_list) <-
    list.files(folder,
               pattern = gsub("\\+", "\\\\+", paste0("_", trt, ".Rds$")),
               full.names = FALSE)

  dat <- map(stan_list, stan_summary)
  names(dat) <- names(stan_list)

  cf_labels <-
    if (is_hier) {
      c("Cure fraction global",
        "Cure fraction OS",
        "Cure fraction PFS")
    } else {
      c("Cure fraction OS",
        "Cure fraction PFS")
    }

  #
  plot_dat <-
    do.call(rbind, dat) %>%
    data.frame(scenario = rep(names(stan_list),
                              each = length(cf_labels)),
               event = cf_labels) %>%
    tidyr::separate(scenario, c(NA, "OS", "PFS", NA)) %>%
    mutate(OS = as.factor(OS),
           PFS = as.factor(PFS))

  plot_dat %>%
    ggplot(aes(x = mean, y = event,
               xmin= `X25.`, xmax = `X75.`,
               colour = OS, shape = PFS)) +
    geom_pointinterval(position = position_dodge(width = 0.8)) +
    geom_point(position = position_dodge(width = 0.8),
               aes(x = mean, y = event,
                   colour = OS, shape = PFS),
               size = 3, inherit.aes = FALSE) +
    scale_shape_manual(values=c(3, 4, 15, 17, 19)) +
    ylab("") +
    xlab("Posterior probability") +
    theme(axis.text = element_text(size = 12)) +
    theme_bw()
}

