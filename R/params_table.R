
#' Posterior parameter summary table for paper
#'
#' @import purrr dplyr
#'
#' params_table(data_dir = "data/independent/cf hier/bg_fixed_hr1",
#' os_distn = "exp",
#' pfs_distn = "weibull",
#' param_names = c("beta_os", "beta_pfs", "cf_os", "cf_pfs", "cf_global"),
#'   # beta_cf_0, beta_cf_1, sigma_cf_os, sigma_cf_pfs,
#'   # shape_os, shape_pfs
#' coeff_rename =
#'   list("beta_os" = c("beta_os0", "beta_os1"),
#'        "beta_pfs" = c("beta_pfs0", "beta_pfs1"))) %>%
#'   knitr::kable(format = "latex")
#'
params_table <- function(data_dir,
                         os_distn,
                         pfs_distn,
                         param_names = c("beta_os", "beta_pfs", "cf_os"),
                         coeff_rename = NULL,
                         tx_names = c("IPILIMUMAB",
                                      "NIVOLUMAB",
                                      "NIVOLUMAB+IPILIMUMAB")) {
  # read in data
  stan_dat <-
    grep(pattern = paste(os_distn, pfs_distn, sep = "_"),
         dir(data_dir, full.names = TRUE),
         value = TRUE) %>%
    sort() %>%
    map(readRDS) %>%
    set_names(tx_names)

  stan_extract <- map(stan_dat, extract)

  # select parameters
  stats <- list()

  for (j in tx_names) {

    stats[[j]] <-
      stan_extract[[j]] %>%
      keep(names(.) %in% param_names) %>%
      map(drop)

    # rename nested parameters
    for (i in names(coeff_rename)) {
      colnames(stats[[j]][[i]]) <- coeff_rename[[i]]
    }

    stats[[j]] <-
      stats[[j]] %>%
      do.call(what = cbind, args = .) %>%
      as_tibble() %>%
      melt() %>%
      group_by(variable) %>%
      summarise(
        tx_name = paste0(round(mean(value),3),
                         " (",
                         round(quantile(value, 0.025),3), ", ",
                         round(quantile(value, 0.975),3), ")")) %>%
      rename(!!j := tx_name)
  }

  # include sd?

  # combine
  plyr::join_all(stats, by = "variable")
}

