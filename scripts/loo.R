
# cross-validation
#
#

library("rstanarm")
library("bayesplot")
library("loo")
library(rethinking)


res_ipi <- readRDS("data/independent/cf separate/stan_exp_exp_IPILIMUMAB.Rds")
res_nivo <- readRDS("data/independent/cf separate/stan_exp_exp_NIVOLUMAB.Rds")
res_nivoipi <- readRDS("data/independent/cf separate/stan_exp_exp_NIVOLUMAB+IPILIMUMAB.Rds")

(loo_ipi <- loo(res_ipi, cores = 2))
(loo_nivo <- loo(res_nivo, cores = 2))
(loo_nivoipi <- loo(res_nivoipi, cores = 2))

plot(loo_res)

# ----

log_lik_ipi <- extract_log_lik(res_ipi)
log_lik_nivo <- extract_log_lik(res_nivo)

waic(log_lik_ipi)
waic(log_lik_nivo)

# ----

# same size data sets so can compare
log_lik_nivo <- log_lik_nivo[, 1:ncol(log_lik_ipi)]

loo_ipi <- loo(log_lik_ipi, cores = 2)
loo_nivo <- loo(log_lik_nivo, cores = 2)


loo_compare(loo_ipi, loo_nivo)
