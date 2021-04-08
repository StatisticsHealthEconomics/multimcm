
# model checking
# cross-validation
#
# at the moment print kable() and then paste
# into Check_mate_analysis.Rmd
# TODO: read in table directly to RMarkdown


library(rstanarm)
library(bayesplot)
library(loo)
library(purrr)
library(dplyr)
library(tibble)


## select model

cf_model <- "cf hier"
# cf_model <- "cf separate"

# bg_model <- "bg_fixed"
bg_model <- "bg_fixed_hr1.63"


## load data

data_dir <- glue::glue("data/independent/{cf_model}/{bg_model}")

scenarios <-
  dir(data_dir) %>%
  gsub("stan_", "", .) %>%
  gsub(".Rds", "", .)

res_ls <-
  dir(data_dir, full.names = TRUE) %>%
  purrr::map(readRDS) %>%
  setNames(scenarios)

# purrr::map(res_ls, loo, cores = 2)
# plot(loo_ipi)


## waic table ----

log_lik <-
  purrr::map(res_ls, extract_log_lik)

tab <-
  purrr::map(log_lik,
             ~round(waic(.x)[["estimates"]], 2)) %>%
  do.call(rbind, .) %>%
  data.frame(Statistic = c("elpd_waic","p_waic","waic"),
             scenarios = rep(scenarios, each = 3)) %>%
  tidyr::separate(scenarios, into = c("OS distn", "PFS distn", "Treatment"), sep = "_") %>%
  relocate(where(is.character)) %>%
  # rownames_to_column(var = "Statistic") %>%
  arrange(Statistic)

knitr::kable(tab, row.names = FALSE)


# ----

# # same size data sets so can compare
# log_lik_nivo <- log_lik_nivo[, 1:ncol(log_lik_ipi)]
#
# loo_ipi <- loo(log_lik_ipi, cores = 2)
# loo_nivo <- loo(log_lik_nivo, cores = 2)
#
# loo_compare(loo_ipi, loo_nivo)


# variance partition coefficients -----------------------------------------

vpc <- function(out) {
  x <- extract(out)
  c(vpc_pfs = round(sd(x$lp_cf_global)^2/(sd(x$lp_cf_global)^2 + sd(x$lp_cf_pfs)^2), 3),
    vpc_os = round(sd(x$lp_cf_global)^2/(sd(x$lp_cf_global)^2 + sd(x$lp_cf_os)^2), 3))
}

knitr::kable(
  cbind(scenarios,
        purrr::map_df(res_hier_fixed, vpc)))

