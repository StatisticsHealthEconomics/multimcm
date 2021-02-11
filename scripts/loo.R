
# model checking
# cross-validation
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


res_hier_fixed <-
  dir("data/independent/cf hier/bg_fixed",
      full.names = TRUE) %>%
  purrr::map(readRDS)

res_sep_distn <-
  dir("data/independent/cf separate",
      full.names = TRUE) %>%
  purrr::map(readRDS)

purrr::map(res_hier_fixed, loo, cores = 2)
purrr::map(res_sep_distn, loo, cores = 2)

# plot(loo_ipi)

# waic table ----

log_lik_hier_fix <- purrr::map(res_hier_fixed, extract_log_lik)
log_lik_sep_distn <- purrr::map(res_hier_fixed, extract_log_lik)

tab <-
  purrr::map(log_lik_hier_fix,
             ~round(waic(.x)[["estimates"]],2)) %>%
  do.call(rbind, .) %>%
  as.data.frame %>%
  rownames_to_column(var = "Statistic") %>%
  arrange(Statistic)

purrr::map(log_lik_sep_distn, waic)

knitr::kable(tab)

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

knitr::kable(purrr::map_df(res_hier_fixed, vpc))


