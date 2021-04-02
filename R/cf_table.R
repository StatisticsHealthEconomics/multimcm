
#' cure fraction summary table
#'
cf_table <- function(data_dir) {

  res_hier_fixed <-
    dir(data_dir, full.names = TRUE) %>%
    purrr::map(readRDS)

  params_hier_fixed <- purrr::map(res_hier_fixed, extract)

  cf_global <- purrr::map(params_hier_fixed, "cf_global")
  cf_pfs <- purrr::map(params_hier_fixed, "cf_pfs")
  cf_os <- purrr::map(params_hier_fixed, "cf_os")

  tab_cf_global <-
    purrr::map(cf_global,
               function(x) {
                 paste0(round(mean(x),3),
                        " (",
                        round(quantile(x, 0.025),3), ", ",
                        round(quantile(x, 0.975),3), ")")}) %>%
    do.call(rbind, .)

  tab_cf_os <-
    purrr::map(cf_os,
               function(x) {
                 paste0(round(mean(x),3),
                        " (",
                        round(quantile(x, 0.025),3), ", ",
                        round(quantile(x, 0.975),3), ")")}) %>%
    do.call(rbind, .)

  tab_cf_pfs <-
    purrr::map(cf_pfs,
               function(x) {
                 paste0(round(mean(x),3),
                        " (",
                        round(quantile(x, 0.025),3), ", ",
                        round(quantile(x, 0.975),3), ")")}) %>%
    do.call(rbind, .)

  scenarios_str <-
    dir(data_dir) %>%
    gsub("stan_", "", .) %>%
    gsub(".Rds", "", .)

  scenarios <-
    scenarios_str %>%
    stringr::str_split("_") %>%
    do.call(rbind, .)

  cbind(ID = 1:length(tab_cf_global),
        scenarios,
        "$cf$ (CrI)" = tab_cf_global,
        "$cf_{OS}$ (CrI)" = tab_cf_os,
        "$cf_{PFS}$ (CrI)" = tab_cf_pfs)
}

