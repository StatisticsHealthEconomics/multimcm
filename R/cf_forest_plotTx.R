
#' Cure fraction forest plot using Stan output
#'
#' Using all treatment Stan model.
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
cf_forest_plotTx <- function(folder = "data/independent/cf hier/bg_fixed_hr1/",
                             is_hier = TRUE) {

  stan_summary <- function(stan_obj, is_hier = TRUE) {
    rstan::summary(stan_obj,
                   par = if (is_hier) {
                     c("cf_global", "cf_os", "cf_pfs")
                   } else{c("cf_os", "cf_pfs")},
                   probs = c(0.25, 0.75))$summary %>%
      as.data.frame() %>%
      mutate(parameter = rownames(.)) %>%
      `rownames<-`(NULL) %>%
      tidyr::separate(parameter, into = c("cf", "tx"), sep = "\\[")}

  keep_files <- !grepl("[IPI | NIVO]", list.files(folder))
  filenames <- list.files(folder, full.names = TRUE)
  filenames <- filenames[keep_files]

  stan_list <- map(filenames, readRDS)

  names(stan_list) <-
    gsub(".Rds", "", list.files(folder, full.names = FALSE)[keep_files])

  dat <- map(stan_list, stan_summary, is_hier = is_hier)
  names(dat) <- names(stan_list)

  plot_dat <-
    do.call(rbind, dat) %>%
    mutate(scenario = rownames(.),
           event = ifelse(cf == "cf_global", "Cure fraction global",
                          ifelse(cf == "cf_os", "Cure fraction OS",
                                 "Cure fraction PFS")),
           treatment = ifelse(tx == "1]", "IPILIMUMAB",
                              ifelse(tx == "2]", "NIVOLUMAB",
                              "NIVOLUMAB+IPILIMUMAB"))) %>%
             tidyr::separate(scenario, c(NA, "OS", "PFS", NA)) %>%
    mutate(OS = as.factor(OS),
           PFS = as.factor(PFS)) %>%
    select(-cf, -tx)

  plot_dat %>%
    ggplot(aes(x = mean, y = event,
               xmin= `25%`, xmax = `75%`,
               colour = OS, shape = PFS)) +
    tidybayes::geom_pointinterval(position = position_dodge(width = 0.8)) +
    geom_point(position = position_dodge(width = 0.8),
               aes(x = mean, y = event,
                   colour = OS, shape = PFS),
               size = 3, inherit.aes = FALSE) +
    facet_grid(. ~ treatment) +
    scale_shape_manual(values=c(3, 4, 15, 17, 19)) +
    ylab("") +
    xlab("Posterior probability") +
    theme(axis.text = element_text(size = 12)) +
    theme_bw()
}

