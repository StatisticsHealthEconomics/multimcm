
#' cure fraction summary table for paper
#'
#' @import purrr dplyr
#'
params_table <- function(data_dir,
                         os_distn,
                         pfs_distn) {
  # read in data
  stan_dat <-
    grep(pattern = paste(os_distn, pfs_distn, sep = "_"),
         dir(data_dir, full.names = TRUE),
         value = TRUE) %>%
    sort() %>%
    map(readRDS) %>%
    set_names("IPILIMUMAB",
              "NIVOLUMAB",
              "NIVOLUMAB+IPILIMUMAB")

  stan_extract <- map(stan_dat, extract)

  # select parameters
  ##TODO: simplify...
  stats <- list()

  stats$ipi <-
    stan_extract$IPILIMUMAB %>%
    keep(names(.) %in% c("beta_os", "beta_pfs")) %>%
    do.call(what = cbind, args = .) %>%
    as_tibble() %>%
    setNames(c("beta_os0", "beta_os1",
               "beta_pfs0", "beta_pfs1")) %>%
    melt() %>%
    group_by(variable) %>%
    summarise(
      ipi = paste0(round(mean(value),3),
                   " (",
                   round(quantile(value, 0.025),3), ", ",
                   round(quantile(value, 0.975),3), ")"))
  stats$nivo <-
    stan_extract$NIVOLUMAB %>%
    keep(names(.) %in% c("beta_os", "beta_pfs")) %>%
    do.call(what = cbind, args = .) %>%
    as_tibble() %>%
    setNames(c("beta_os0", "beta_os1",
               "beta_pfs0", "beta_pfs1")) %>%
    melt() %>%
    group_by(variable) %>%
    summarise(
      nivo = paste0(round(mean(value),3),
                    " (",
                    round(quantile(value, 0.025),3), ", ",
                    round(quantile(value, 0.975),3), ")"))
  stats$nivo_ipi <-
    stan_extract$`NIVOLUMAB+IPILIMUMAB` %>%
    keep(names(.) %in% c("beta_os", "beta_pfs")) %>%
    do.call(what = cbind, args = .) %>%
    as_tibble() %>%
    setNames(c("beta_os0", "beta_os1",
               "beta_pfs0", "beta_pfs1")) %>%
    melt() %>%
    group_by(variable) %>%
    summarise(
      nivo_ipi = paste0(round(mean(value),3),
                        " (",
                        round(quantile(value, 0.025),3), ", ",
                        round(quantile(value, 0.975),3), ")"))

  # include sd

  # combine
  plyr::join_all(stats, by = "variable")

}

