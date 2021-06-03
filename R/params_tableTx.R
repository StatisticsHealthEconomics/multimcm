
#' Posterior parameter summary table for paper
#'
#' For single treatment Stan output
#'
#' @import purrr dplyr
#'
# params_tableTx(
#   data_dir = "data/independent/cf hier/bg_fixed_hr1",
#   os_distn = "exp",
#   pfs_distn = "exp",
#   param_names = c("beta_os", "beta_pfs", "cf_os", "cf_pfs", "cf_global", "alpha", "sd_cf"),
#    #shape_os, shape_pfs # if present
#    coeff_rename =
#      list("beta_os" = c("beta_os0", "beta_os1"),
#           "beta_pfs" = c("beta_pfs0", "beta_pfs1"),
#             "cf_os" = c("cf_os1", "cf_os2", "cf_os3"),
#            "cf_pfs" = c("cf_pfs1", "cf_pfs2", "cf_pfs3"),
#            "cf_global" = c("cf_global1", "cf_global2", "cf_global3"),
#           "alpha" = c("beta_cf_1", "beta_cf_2", "beta_cf_3"),
#           "sd_cf" = c("sd_cf_1", "sd_cf_2", "sd_cf_3"))
#   ) %>%
#     knitr::kable(format = "latex")
#'
params_tableTx <- function(data_dir,
                           os_distn,
                           pfs_distn,
                           param_names = c("beta_os", "beta_pfs", "cf_os"),
                           coeff_rename = NULL) {
  # read in data
  stan_dat <-
    readRDS(grep(pattern = paste0(os_distn, "_", pfs_distn, ".Rds"),
                 dir(data_dir, full.names = TRUE),
                 value = TRUE))

  stan_extract <- extract(stan_dat)

  stats <-
    stan_extract %>%
    keep(names(.) %in% param_names) %>%
    drop()

  # rename nested parameters
  for (i in names(coeff_rename)) {
    colnames(stats[[i]]) <- coeff_rename[[i]]
  }

  stats %>%
    do.call(what = cbind, args = .) %>%
    as_tibble() %>%
    melt() %>%
    group_by(variable) %>%
    summarise(
      "mean" = round(mean(value),3),
      "95\\% CrI" = paste0(
        " (",
        round(quantile(value, 0.025),3), ", ",
        round(quantile(value, 0.975),3), ")"))
}

