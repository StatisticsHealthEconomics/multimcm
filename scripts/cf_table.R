
# cure fraction output tables
#
# at the moment print kable() and then paste
# into Check_mate_analysis.Rmd
# TODO: read in table directly to RMarkdown


library(rstan)
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

scenarios_str <-
  dir("data/independent/cf hier/bg_fixed") %>%
  gsub("stan_", "", .) %>%
  gsub(".Rds", "", .)

scenarios <-
  scenarios_str %>%
  stringr::str_split("_") %>%
  do.call(rbind, .)

knitr::kable(
  cbind(ID = 1:length(tab_cf_global),
        scenarios,
        "$cf$ (CrI)" = tab_cf_global,
        "$cf_{OS}$ (CrI)" = tab_cf_os,
        "$cf_{PFS}$ (CrI)" = tab_cf_pfs))


# forest plot ----
##TODO: automate names...

cf_global[[12]] <- cf_global[[1]][1:90, ]
cf_os[[12]] <- cf_global[[1]][1:90, ]
cf_pfs[[12]] <- cf_global[[1]][1:90, ]


yy <- gsub("exp_exp_", "Exponential ", scenarios_str)
yy <- gsub("gompertz_gompertz_", "Gompertz ", yy)
yy <- gsub("weibull_weibull_", "Weibull ", yy)
yy <- gsub("loglogistic_loglogistic_", "Log-logistic ", yy)
yy <- gsub("lognormal_lognormal_", "Log-Normal ", yy)

xx <-
  cbind(
    as.data.frame(cf_global) %>% setNames(paste("cf global", scenarios_str)),
    as.data.frame(cf_os) %>% setNames(paste("cf os", scenarios_str)),
    as.data.frame(cf_pfs) %>% setNames(paste("cf pfs", scenarios_str))) #%>%
  # setNames(c("cf_global_ipi_exp", "cf_global_nivo_exp", "cf_global_both_exp", "cf_global_ipi_llog", "cf_global_nivo_llog", "cf_global_both_llog",
  #            "cf_os_ipi_exp", "cf_os_nivo_exp", "cf_os_both_exp", "cf_os_ipi_llog", "cf_os_nivo_llog", "cf_os_both_llog",
  #            "cf_pfs_ipi_exp", "cf_pfs_nivo_exp", "cf_pfs_both_exp", "cf_pfs_ipi_llog", "cf_pfs_nivo_llog", "cf_pfs_both_llog"))

mcmc_intervals(xx) + xlim(0, 0.65)

ggsave(filename = "plots/cf_forest_plot.png")

as.data.frame(cf_global) %>%
  setNames(scenarios_str) %>%
  mcmc_intervals() +
  xlim(0, 0.65)
ggsave(filename = "plots/cf_global_forest_plot.png")

as.data.frame(cf_os) %>%
  setNames(scenarios_str) %>%
  mcmc_intervals() +
  xlim(0, 0.65)
ggsave(filename = "plots/cf_os_forest_plot.png")

as.data.frame(cf_pfs) %>%
  setNames(scenarios_str) %>%
  mcmc_intervals() +
  xlim(0, 0.65)
ggsave(filename = "plots/cf_pfs_forest_plot.png")


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



