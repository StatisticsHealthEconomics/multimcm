
#' Cure fraction forest plot using Stan output
#'
#' Using all treatment Stan model.
#'
#' @param folder String of location directory
#'
#' @import dplyr ggplot2 tidybayes purrr
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#'  # independent model
#'  fp_sep <- cf_forest_plot("data/separate/")
#'  #ggsave(fp_sep, filename = "plots/forest_plot_sep_multimcm.png", dpi = 640, width = 12, height = 8)
#'
#'  # hierarchical model
#'  fp_hier <- cf_forest_plot("data/")
#'  #ggsave(fp_hier, filename = "plots/forest_plot_multimcm.png", dpi = 640, width = 12, height = 8)
#' }
cf_forest_plot <- function(folder = "data/") {

  # read in data
  filenames <- list.files(folder, full.names = TRUE)
  keep_files <- grepl(pattern = "bmcm_stan_[a-z]+_[a-z]+.Rds", filenames)
  filenames <- filenames[keep_files]
  stan_list <- map(filenames, readRDS)

  names(stan_list) <-
    gsub(".Rds", "", list.files(folder, full.names = FALSE)[keep_files])

  dat <- map(stan_list, stan_summary)
  names(dat) <- names(stan_list)

  plot_dat <-
    do.call(rbind, dat) %>%
    mutate(scenario = gsub(".[0-9]+", "", rownames(.)),
           scenario = gsub("bmcm_stan_", "", scenario),
           event = ifelse(cf == "global", "Cure fraction global",
                          ifelse(cf == 1, "Cure fraction OS",
                                 "Cure fraction PFS")),
           treatment = ifelse(tx == 1, "Ipilimumab",
                              ifelse(tx == 2, "Nivolumab",
                                     "Ipilimumab + Nivolumab")),
           treatment = factor(treatment,
                              levels = c("Ipilimumab", "Nivolumab", "Ipilimumab + Nivolumab"))) %>%
    tidyr::separate(scenario, c("OS", "PFS")) %>%
    `rownames<-`(NULL) %>%
    mutate(OS = as.factor(OS),
           PFS = as.factor(PFS)) %>%
    select(-cf, -tx)

  plot_dat %>%
    ggplot(aes(x = mean, y = event,
               xmin = `2.5%`, xmax = `97.5%`,
               colour = OS, shape = PFS)) +
    tidybayes::geom_pointinterval(position = position_dodge(width = 1)) +
    geom_point(position = position_dodge(width = 1),
               aes(x = mean, y = event,
                   colour = OS, shape = PFS),
               size = 3, inherit.aes = FALSE) +
    facet_grid(. ~ treatment) +
    scale_shape_manual(values = c(3, 4, 15, 17, 19)) +
    ylab("") +
    xlab("Posterior probability") +
    theme(axis.text = element_text(size = 12)) +
    xlim(0, 0.65) +
    theme_bw() +
    theme(panel.grid.major.y = element_blank()) +
    geom_hline(aes(yintercept = 1.5), col = "lightgrey") +
    geom_hline(aes(yintercept = 2.5), col = "lightgrey") +
    theme(text = element_text(size = 20))
}


#
stan_summary <- function(stan_obj) {

  cf_names <- names(stan_obj$output)[
    grepl(pattern = "^cf_", names(stan_obj$output))]

  rstan::summary(stan_obj$output,
                 par = cf_names,
                 probs = c(0.025, 0.975))$summary %>%  # 95%
    as.data.frame() %>%
    mutate(parameter = rownames(.)) %>%
    `rownames<-`(NULL) %>%
    tidyr::separate(parameter, into = c("cf", "tx"), sep = "\\[") |>
    mutate(tx = gsub(pattern = "]", "", tx),
           cf = gsub(pattern = "cf_", "", cf))
}


