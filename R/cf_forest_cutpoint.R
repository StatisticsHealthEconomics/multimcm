
#' Cure fraction forest plot using cut-point Stan output
#'
#' Using all treatment Stan model.
#'
#' @param folder String of location
#' @param is_hier Is hierarchical model (or separate)?
#'
#' @importFrom epicontacts, adegenet
#' @import dplyr ggplot2 tidybayes purrr
#'
#' @return
#' @export
#'
cf_forest_cutpoint <- function(distns = list(c("exp", "exp"),
                                             c("lognormal", "lognormal")),
                               folder = "data/independent/cf separate/bg_fixed_hr1",
                               save_name = c("_12", "_30", "_100"),
                               is_hier = TRUE) {
  summary_tabs <- list()

  for (cpt in save_name) {
    for (d in distns) {
      i <- d[1]; print(i)
      j <- d[2]; print(j)

      dist_names <- paste(i,j)

      summary_tabs[[dist_names]][[cpt]] <-
        readRDS(glue::glue("{folder}/bmcm_stan_{i}_{j}{cpt}.Rds")) %>%
        stan_summary() %>%
        cbind(distns = dist_names, cpt = cpt)
    }
  }

  plot_dat <-
    flatten_dfr(summary_tabs) %>%
    mutate(scenario = rownames(.),
           event = ifelse(cf == "global", "Cure fraction global",
                          ifelse(cf == "1", "Cure fraction OS",
                                 "Cure fraction PFS")),
           treatment = ifelse(tx == "1", "Ipilimumab",
                              ifelse(tx == "2", "Nivolumab",
                                     "Nivolumab + Ipilimumab"))) %>%
    mutate(distns = as.factor(distns),
           cpt = ifelse(cpt == "_12", "12 months",
                        ifelse(cpt == "_30", "30 months",
                               "Complete")),
           cpt = as.factor(cpt)) %>%
    select(-cf, -tx) %>%
    rename(`Cut-point` = cpt,
           `Distribution` = distns)

  plot_dat %>%
    ggplot(aes(x = mean, y = event,
               xmin = `2.5%`, xmax = `97.5%`,
               colour = `Distribution`,
               shape = `Cut-point`,
               size = 2)) +
    tidybayes::geom_pointinterval(position = position_dodge(width = 0.8)) +
    geom_point(position = position_dodge(width = 0.8),
               aes(x = mean, y = event,
                   colour = `Distribution`, shape = `Cut-point`),
               size = 5, inherit.aes = FALSE) +
    facet_grid(. ~ treatment) +
    scale_shape_manual(values = c(3, 4, 15, 17, 19)) +
    ylab("") +
    xlab("Posterior probability") +
    xlim(0, 0.8) +
    theme_bw() +
    theme(axis.text = element_text(size = 20),
          strip.text.x = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          panel.spacing = unit(2, "lines"))
}

