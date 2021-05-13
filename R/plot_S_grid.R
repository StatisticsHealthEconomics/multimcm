
#' Plot survival curves on a grid
#'
#' Overlaid Kaplan-Meier
#'
#' @param distn Vector of distribution names;
#'              "exp", "weibull", "lognormal", "gompertz", "loglogistic"
#' @param folder Location of data
#' @param trt Treatment name; string
#' @param data Survival input data frame
#'
#' @import gridExtra purrr grid survival
#'
plot_S_grid <- function(distns = c("exp", "weibull", "lognormal", "gompertz", "loglogistic"),
                        folder = "data/independent/cf hier/bg_fixed_hr1",
                        trt = "NIVOLUMAB+IPILIMUMAB",
                        data = surv_input_data) {

  source("R/grid_arrange_shared_legend.R")

  fit_os <- survfit(Surv(os, os_event) ~ 1,
                    data = filter(data, TRTA == trt))
  fit_pfs <- survfit(Surv(pfs, pfs_event) ~ 1,
                     data = filter(data, TRTA == trt))
  km_data <-
    rbind(
      data.frame(Tx = trt,
                 event_type = "os",
                 time = fit_os$time,
                 surv = fit_os$surv),
      data.frame(Tx = trt,
                 event_type = "pfs",
                 time = fit_pfs$time,
                 surv = fit_pfs$surv))

  legend_txt <-
    c("Background", "Uncured OS", "Sample OS", "Uncured PFS", "Sample PFS")
  dat <- list()

  for (i in distns) {
    for (j in distns) {
      dat[[i]][[j]] <-
        readRDS(glue::glue("{folder}/stan_{i}_{j}_{trt}.Rds")) %>%
        list(trt = .) %>%
        plot_S_joint(stan_list = .,
                     facet = FALSE,
                     annot_cf = FALSE) +
        ggtitle(glue::glue("OS: {i}, PFS: {j}")) +
        theme_bw() +
        # Kaplan-Meier
        geom_line(aes(x = time, y = surv),
                  data = km_data[km_data$event_type == "pfs", ],
                  lwd = 1,
                  inherit.aes = FALSE) +
        geom_line(aes(x = time, y = surv),
                  data = km_data[km_data$event_type == "os", ],
                  lwd = 1,
                  inherit.aes = FALSE) +
        xlim(0, 60) +
        guides(fill = guide_legend(title = ""),
               color = guide_legend(title = "")) +
        scale_fill_hue(labels = legend_txt) +
        scale_colour_hue(labels = legend_txt)
    }
  }

  do.call("grid_arrange_shared_legend",
          c(flatten(dat), nrow = 5, ncol = 5))
}

