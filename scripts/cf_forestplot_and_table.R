
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
library(glue)

source("R/cf_table.R")

cf_model <- "cf hier"
# cf_model <- "cf separate"

# bg_model <- "bg_fixed"
bg_model <- "bg_fixed_hr1.63"

data_dir <- glue("data/independent/{cf_model}/{bg_model}")


cf_table(data_dir) %>%
  knitr::kable()


# forest plot ----
##TODO: automate names...

scenarios_str <-
  dir(data_dir) %>%
  gsub("stan_", "", .) %>%
  gsub(".Rds", "", .)

res_ls <-
  dir(data_dir, full.names = TRUE) %>%
  purrr::map(readRDS)

params_hier_fixed <- purrr::map(res_ls, extract)

cf_global <- purrr::map(params_hier_fixed, "cf_global")
cf_pfs <- purrr::map(params_hier_fixed, "cf_pfs")
cf_os <- purrr::map(params_hier_fixed, "cf_os")

# cf_global[[12]] <- cf_global[[1]][1:90, ]
# cf_os[[12]] <- cf_global[[1]][1:90, ]
# cf_pfs[[12]] <- cf_global[[1]][1:90, ]

# clean names
distn_tx <- gsub("exp_exp_", "Exponential ", scenarios_str)
distn_tx <- gsub("gompertz_gompertz_", "Gompertz ", distn_tx)
distn_tx <- gsub("weibull_weibull_", "Weibull ", distn_tx)
distn_tx <- gsub("loglogistic_loglogistic_", "Log-logistic ", distn_tx)
distn_tx <- gsub("lognormal_lognormal_", "Log-Normal ", distn_tx)
distn_tx <- gsub("NIVOLUMAB\\+IPILIMUMAB", "Combined", distn_tx)
distn_tx <- gsub("NIVOLUMAB", "Nivolumab", distn_tx)
distn_tx <- gsub("IPILIMUMAB", "Ipilimumab", distn_tx)

xx <-
  cbind(
    as.data.frame(cf_global) %>% setNames(paste("cf global", distn_tx)),
    as.data.frame(cf_os) %>% setNames(paste("cf os", distn_tx)),
    as.data.frame(cf_pfs) %>% setNames(paste("cf pfs", distn_tx)))

mcmc_intervals(xx) #+ xlim(0, 0.65)

ggsave(filename = glue("plots/{cf_model}_{bg_model}_forest_plot.png"))

# as.data.frame(cf_global) %>%
#   setNames(distn_tx) %>%
#   mcmc_intervals() #+
#   # xlim(0, 0.65)
# ggsave(filename = glue("plots/{cf_model}_{bg_model}_global_forest_plot.png"))
#
# as.data.frame(cf_os) %>%
#   setNames(distn_tx) %>%
#   mcmc_intervals() +
#   xlim(0, 0.65)
# ggsave(filename = glue("plots/{cf_model}_os_forest_plot.png"))
#
# as.data.frame(cf_pfs) %>%
#   setNames(distn_tx) %>%
#   mcmc_intervals() +
#   xlim(0, 0.65)
# ggsave(filename = glue("plots/{cf_model}_pfs_forest_plot.png"))



##REMOVE?
## joint, separate ----

res_joint_distn <-
  dir(data_dir,
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



