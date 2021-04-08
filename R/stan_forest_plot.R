
#' Forest plot using Stan output
#'
#' @param stan_list Named list of Stan output
#' @param save
#'
#' @importFrom epicontacts, adegenet
#' @import dplyr ggplot2 tidybayes purrr
#'
#' @return
#' @export
#'
stan_forest_plot <- function(stan_list,
                             save = FALSE) {

  stan_summary <- function(stan_obj) {
    summary(stan_obj,
            par = c("cf_global", "cf_os", "cf_pfs"),
            probs = c(0.25, 0.75))$summary}

  dat <- map(stan_list, stan_summary)
  names(dat) <- names(stan_list)

  plot_dat <-
    do.call(rbind, dat) %>%
    ##TODO:
    # hierarchical
    # data.frame(scenario = rep(names(stan_list), each = 3),
    #            event = c("cf_global", "cf_os", "cf_pfs")) %>%
    # separate
    data.frame(scenario = rep(names(stan_list), each = 2),
               event = c("cf_os", "cf_pfs")) %>%
    # pretty rename
    mutate(Model =
             ifelse(grepl("exp", scenario),
                    "Exponential",
                    ifelse(grepl("gompertz", scenario),
                           "Gompertz",
                           ifelse(grepl("loglogistic", scenario),
                                  "Log-logistic",
                                  ifelse(grepl("lognormal", scenario),
                                         "Log-Normal", "Weibull")))),
           Treatment =
             ifelse(grepl("NIVOLUMAB$", scenario),
                    "Nivo",
                    ifelse(grepl("NIVOLUMAB\\+IPILIMUMAB", scenario),
                           "Combined", "Ipi")))

  out <-
    plot_dat %>%
    ggplot(aes(x = mean, y = event, xmin= `X25.`, xmax = `X75.`, colour = Model, shape = Treatment)) +
    ##TODO: for different OS and PFS distns
    # colour = distn_os, shape = distn_pfs)) +
    geom_pointinterval(position = position_dodge(width = 0.5)) +
    geom_point(position = position_dodge(width = 0.5),
               aes(x = mean, y = event, colour = Model, shape = Treatment),
               size = 3, inherit.aes = FALSE) +
    # scale_shape_manual(values=c(3, 16, 17))+
    xlab("Posterior probability") +
    theme(axis.text = element_text(size = 12)) +
    scale_y_discrete("",
                     # labels = c("Cure fraction global", "Cure fraction OS", "Cure fraction PFS")) +
                     labels = c("Cure fraction OS", "Cure fraction PFS")) +
    theme_bw()

  out
}

