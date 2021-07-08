
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
                               save_name = c("_cpt_12m", "_cpt_30m", ""),
                               is_hier = TRUE) {
  summary_tabs <- list()
  
  for (cpt in save_name) {
    for (d in distns) {
      i <- d[1]; print(i)
      j <- d[2]; print(j)
      
      dist_names <- paste(i,j)
      
      summary_tabs[[dist_names]][[cpt]] <-
        readRDS(glue::glue("{folder}/stan_{i}_{j}{cpt}.Rds")) %>% 
        stan_summary(is_hier) %>% 
        cbind(distns = dist_names, cpt = cpt)
    }
  }
  
  plot_dat <-
    flatten_dfr(summary_tabs) %>%
    mutate(scenario = rownames(.),
           event = ifelse(cf == "cf_global", "Cure fraction global",
                          ifelse(cf == "cf_os", "Cure fraction OS",
                                 "Cure fraction PFS")),
           treatment = ifelse(tx == "1]", "Ipilimumab",
                              ifelse(tx == "2]", "Nivolumab",
                                     "Nivolumab + Ipilimumab"))) %>%
    mutate(cpt = ifelse(cpt == "", "Complete", cpt),
           distns = as.factor(distns),
           cpt = as.factor(cpt)) %>%
    select(-cf, -tx)
  
  plot_dat %>%
    ggplot(aes(x = mean, y = event,
               xmin = `2.5%`, xmax = `97.5%`,
               colour = distns, shape = cpt)) +
    tidybayes::geom_pointinterval(position = position_dodge(width = 0.8)) +
    geom_point(position = position_dodge(width = 0.8),
               aes(x = mean, y = event,
                   colour = distns, shape = cpt),
               size = 3, inherit.aes = FALSE) +
    facet_grid(. ~ treatment) +
    scale_shape_manual(values = c(3, 4, 15, 17, 19)) +
    ylab("") +
    xlab("Posterior probability") +
    theme(axis.text = element_text(size = 12)) +
    theme_bw()
}

