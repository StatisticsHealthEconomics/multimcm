
#' Plot survival curves on a grid of distributions
#'
#' For a all treatments.
#'
#' @param distn Vector of distribution names;
#'              "exp", "weibull", "lognormal", "gompertz", "loglogistic"
#' @param folder Location of data
#' @param data Survival input data frame
#'
#' @import gridExtra purrr grid survival dplyr
#'
plot_S_gridTx <- function(distns = c("exp", "weibull", "gompertz", "loglogistic", "lognormal"),
                          folder = "data/independent/cf hier/bg_fixed_hr1",
                          data = surv_input_data) {

  legend_txt <-
    c("Background", "Uncured OS", "Sample OS", "Uncured PFS", "Sample PFS")

  num_distns <- length(distns)

  dat <- list()

  for (i in distns) {
    for (j in distns) {
      ##TODO: which missing?...
      print(i)
      print(j)
      dat[[i]][[j]] <-
        readRDS(glue::glue("{folder}/stan_{i}_{j}.Rds")) %>%
        plot_S_jointTx(stan_out = .,
                       facet = TRUE,
                       annot_cf = FALSE,
                       data = data) +
        ggtitle(glue::glue("OS: {i}, PFS: {j}"))
    }
  }

  do.call("grid_arrange_shared_legend",
          c(flatten(dat), nrow = num_distns, ncol = num_distns))
}

