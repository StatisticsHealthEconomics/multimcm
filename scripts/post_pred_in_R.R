
# generate survival curves from
# Stan output


library(glue)


distn <- "exp"
# distn <- "weibull"
# distn <- "loglogistic"

tx <- "IPILIMUMAB"
# tx <- "NIVOLUMAB"
# tx <- "NIVOLUMAB+IPILIMUMAB"


stan_distn_tx <-
  readRDS(glue("data/independent/cf hier/bg_fixed/stan_{distn}_{distn}_{tx}.Rds"))

dat <- extract(stan_distn_tx)

S <- list(os = list(),
          pfs = list(),
          bg = list())

t_max <- 240
n_sim <- 90

for (i in 1:n_sim) {
  S$os[[i]] <- pexp(1:t_max, rate = dat$mean_os[i], lower.tail = FALSE)
  S$pfs[[i]] <- pexp(1:t_max, rate = dat$mean_pfs[i], lower.tail = FALSE)
  S$bg[[i]] <- pexp(1:t_max, rate = dat$mean_bg[i], lower.tail = FALSE)
}

out <- list()

for (i in names(S)) {
  out[[i]] <-
    do.call(cbind, S[[i]])
}

out$pred <-
  t(t(out$os)*(1 - c(dat$cf_os)) + t(out$bg)*c(dat$cf_os))

res <- list()

for (i in names(out)) {
  res[[i]] <-
    out[[i]] %>%
    as_tibble() %>%
    rowwise() %>%
    transmute(mean = mean(c_across(1:n_sim)),
              u95 = quantile(c_across(1:n_sim), 0.975),
              l95 = quantile(c_across(1:n_sim), 0.025))
}


## plots

plot(res$pred$mean, type = "l", ylim = c(0,1))
lines(res$pred$u95, type = "l")
lines(res$pred$l95, type = "l")

plot(res$os$mean, type = "l")
lines(res$os$l95, type = "l")
lines(res$os$u95, type = "l")


saveRDS(res, file = glue("data/{distn}_{distn}_{tx}_t240.Rds"))

