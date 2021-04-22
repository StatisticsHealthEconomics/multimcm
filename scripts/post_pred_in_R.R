
# generate survival curves from
# Stan output


library(glue)


# distn <- "exp"
distn <- "weibull"
# distn <- "loglogistic"

# tx <- "IPILIMUMAB"
# tx <- "NIVOLUMAB"
tx <- "NIVOLUMAB+IPILIMUMAB"


stan_distn_tx <-
  readRDS(glue("data/independent/cf hier/bg_fixed/stan_{distn}_{distn}_{tx}.Rds"))

dat <- extract(stan_distn_tx)

S <- list(os = list(),
          pfs = list(),
          bg = list())

t_max <- 240
n_sim <- 90

for (i in 1:n_sim) {
  if (distn == "exp") {
    S$os[[i]] <-
      pexp(1:t_max,
           rate = dat$mean_os[i],
           lower.tail = FALSE)
    S$pfs[[i]] <-
      pexp(1:t_max,
           rate = dat$mean_pfs[i],
           lower.tail = FALSE)
    S$bg[[i]] <-
      pexp(1:t_max,
           rate = dat$mean_bg[i],
           lower.tail = FALSE)
  } else if (distn == "weibull") {
    S$os[[i]] <-
      pweibull(
        1:t_max,
        shape = dat$shape_os[i],
        scale = dat$mean_os[i],
        lower.tail = FALSE)
    S$pfs[[i]] <-
      pweibull(
        1:t_max,
        shape = dat$shape_pfs[i],
        scale = dat$mean_pfs[i],
        lower.tail = FALSE)
    S$bg[[i]] <-
      pexp(1:t_max,
           rate = dat$mean_bg[i],
           lower.tail = FALSE)
  } else if (distn == "loglogistic") {
    S$os[[i]] <-
      actuar::pllogis(
        1:t_max,
        shape = dat$shape_os[i],
        scale = dat$mean_os[i],
        lower.tail = FALSE)
    S$pfs[[i]] <-
      actuar::pllogis(
        1:t_max,
        shape = dat$shape_pfs[i],
        scale = dat$mean_pfs[i],
        lower.tail = FALSE)
    S$bg[[i]] <-
      pexp(1:t_max,
           rate = dat$mean_bg[i],
           lower.tail = FALSE)
  }
}

out <- list()

for (i in names(S)) {
  out[[i]] <-
    do.call(cbind, S[[i]])
}

out$pred_os <-
  t(t(out$os)*(1 - c(dat$cf_os)) + t(out$bg)*c(dat$cf_os))

out$pred_pfs <-
  t(t(out$pfs)*(1 - c(dat$cf_pfs)) + t(out$bg)*c(dat$cf_pfs))

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

plot(res$pred_pfs$mean, type = "l", ylim = c(0,1))
lines(res$pred_pfs$u95, type = "l")
lines(res$pred_pfs$l95, type = "l")

# check against original
lines(colMeans(dat$S_pfs_pred), type = "l", col = "red")


saveRDS(res, file = glue("data/{distn}_{distn}_{tx}_t240.Rds"))

