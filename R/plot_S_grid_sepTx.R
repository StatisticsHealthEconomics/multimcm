
#' Plot survival curves on a grid of distributions
#'
#' For a all treatments.
#'
#' @param distn Vector or list of pairs of distribution names;
#'              "exp", "weibull", "lognormal", "gompertz", "loglogistic"
#' @param folder Location of data without final /
#' @param data Survival input data frame
#' @param save_name additional text to file name
#' @param n_dim Plotting gird dimnensions
#'
#' @import gridExtra purrr grid survival dplyr
#'
plot_S_grid_sepTx <- function(distns = c("exp", "weibull", "gompertz", "loglogistic", "lognormal"),
                              folder = "C:/Users/n8tha/Documents/R/rstanbmcm/vignettes/checkmate_report_data/bg_fixed",
                              data,
                              save_name = "",
                              n_dim = NA) {
  legend_txt <-
    c("Background", "Uncured OS", "Sample OS", "Uncured PFS", "Sample PFS")

  n_distn <- length(distns)
  n_dim <- c(3,2)

  # if (!is.list(distns)) {
  #   distns <- split(expand.grid(distns, distns),
  #                   1:(n_distn*n_distn))
  #   n_dim <- c(n_distn, n_distn)
  # }
  #
  # # guess plotting grid dimensions
  # if (any(is.na(dim))) {
  #   n_dim <-
  #     do.call(rbind.data.frame, distns) %>%
  #     purrr::map_dbl(~ length(unique(.x)))
  # }

  dat <- list()

  for (s in seq_len(n_distn)) {
    d <- distns[[s]]
    i <- unlist(d[1]); print(i)
    j <- unlist(d[2]); print(j)

    stan_dat <- list(
      readRDS(glue::glue("{folder}/stan_{i}_{j}_IPILIMUMAB.Rds")),
      readRDS(glue::glue("{folder}/stan_{i}_{j}_NIVOLUMAB.Rds")),
      readRDS(glue::glue("{folder}/stan_{i}_{j}_NIVOLUMAB+IPILIMUMAB.Rds")))

    dat[[i]][[j]] <-
      stan_dat %>%
      plot_S_sepTx(stan_out = .,
                   facet = TRUE,
                   annot_cf = FALSE,
                   data = NA) +
      ggtitle(glue::glue("({as.roman(s)})"))
  }
  do.call("grid_arrange_shared_legend",
          c(flatten(dat), nrow = n_dim[1], ncol = n_dim[2]))
}

