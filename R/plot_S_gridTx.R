
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
plot_S_gridTx <- function(distns = c("exp", "weibull", "gompertz", "loglogistic", "lognormal"),
                          folder = "data/independent/cf hier/bg_fixed_hr1",
                          data,
                          save_name = "",
                          n_dim = NA) {
  legend_txt <-
    c("Background", "Uncured OS", "Sample OS", "Uncured PFS", "Sample PFS")
  n_distn <- length(distns)

  if (!is.list(distns)) {
    distns <- split(expand.grid(distns, distns),
                    1:(n_distn*n_distn))
    n_dim <- c(n_distn, n_distn)
  }

  # guess plotting grid dimensions
  if (any(is.na(dim))) {
    n_dim <-
      do.call(rbind.data.frame, distns) %>%
      purrr::map_dbl(~ length(unique(.x)))
  }

  dat <- vector("list", n_distn)

  for (s in seq_len(n_distn)) {
    d <- distns[[s]]
    i <- unlist(d[1]); print(i)
    j <- unlist(d[2]); print(j)

    dat[[i]][[j]] <-
      readRDS(glue::glue("{folder}/stan_{i}_{j}{save_name}.Rds")) %>%
      plot_S_jointTx(stan_out = .,
                     facet = TRUE,
                     annot_cf = FALSE,
                     data = data) +
      # ggtitle(glue::glue("OS: {i}, PFS: {j}"))
      ggtitle(glue::glue("({as.roman(s)})"))
  }
  do.call("grid_arrange_shared_legend",
          c(flatten(dat), nrow = n_dim[1], ncol = n_dim[2]))
}

