
# cure fraction prior histogram

library(purrr)
library(survival)


n <- 10000

# sample global cure fraction prior on beta
cf <- boot::inv.logit(rnorm(n, -0.1, sd = sqrt(0.2)))

# end-point specific cure fraction variance prior
# truncated normal
endpt_var <- msm::rtnorm(n, 0, sd = sqrt(2.5), lower = 0)

cf_endpt_logit <- boot::inv.logit(rnorm(n, boot::logit(cf), sd = sqrt(endpt_var)))
cf_endpt <- boot::inv.logit(rnorm(n, cf, sd = sqrt(endpt_var)))


# plots

par(mfrow = c(1,2))
hist(cf,
     breaks = 20, xlim = c(0,1),
     main = "", xlab = "i)",
     freq = FALSE)
lines(density(cf), lwd = 2)
abline(v=0.5, col = "red")
# hist(cf_endpt,
#      breaks = 30,
#      xlim = c(0,1),
#      main = "", xlab = "")
# abline(v=0.5, col = "red")
hist(cf_endpt_logit,
     breaks = 30,
     xlim = c(0,1),
     main = "", xlab = "ii)",
     freq = FALSE)
lines(density(cf_endpt_logit), lwd = 2)
abline(v=0.5, col = "red")


# stats

sum(cf > 0.6)/n
sum(cf > 0.7)/n

# survival curves
# centered
ns <- 500
t_cf <- list()
t_endpt <- list()

for (j in 1:30) {
  t_cf[[j]] <- NA
  t_endpt[[j]] <- NA

  for (i in 1:ns) {
    t_cf[[j]][i] <-
      if (runif(1) > cf[j]) {
        rexp(1,1)
      } else {100}

    t_endpt[[j]][i] <-
      if (runif(1) > cf_endpt_logit[j]) {
        rexp(1,1)
      } else {100}
  }
}

par(mfrow = c(1,2))

km_fit <- map(t_cf, function(x) survfit(Surv(x, rep(1, ns)) ~ 1))
plot(km_fit[[1]], xlim = c(0,5), conf.int = FALSE, col = "grey", xlab = "i)", ylab = "S")
map(km_fit, lines, conf.int = FALSE, col = "grey")

km_fit <- map(t_endpt, function(x) survfit(Surv(x, rep(1, ns)) ~ 1))
plot(km_fit[[1]], xlim = c(0,5), conf.int = FALSE, col = "grey", xlab = "ii)", ylab = "S")
map(km_fit, lines, conf.int = FALSE, col = "grey")

