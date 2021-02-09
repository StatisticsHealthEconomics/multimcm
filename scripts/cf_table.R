
# cure fraction output tables
#
# at the moment print kable() and then paste
# into Check_mate_analysis.Rmd
# TODO: read in table directly to RMarkdown


library(rstanarm)
library(bayesplot)
library(loo)
library(rethinking)
library(purrr)
library(dplyr)
library(tibble)


## hierarchical

res_hier_fixed <-
  dir("data/independent/cf hier/bg_fixed",
      full.names = TRUE) %>%
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


knitr::kable(
  cbind(tab_cf_global,
        tab_cf_os,
        tab_cf_pfs))


## joint, separate


res_joint_distn <-
  dir("data/joint/cf separate",
      full.names = TRUE) %>%
  purrr::map(readRDS)

params_joint <- purrr::map(res_joint_distn, extract)

beta_joint <- purrr::map(params_joint, "beta_joint")
cf_pfs <- purrr::map(params_joint, "cf_pfs")
cf_os <- purrr::map(params_joint, "cf_os")

tab_beta_joint <-
  purrr::map(beta_joint,
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


knitr::kable(
  cbind(tab_cf_os,
        tab_cf_pfs,
        tab_beta_joint))



