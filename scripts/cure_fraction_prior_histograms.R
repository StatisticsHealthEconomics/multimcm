
#

n <- 10000
cf <- boot::inv.logit(rnorm(n, -0.1, sd = sqrt(0.2)))

endpt_var <- msm::rtnorm(n, 0, sd = sqrt(2.5), lower = 0)

cf_endpt_logit <- boot::inv.logit(rnorm(n, boot::logit(cf), sd = sqrt(endpt_var)))
cf_endpt <- boot::inv.logit(rnorm(n, cf, sd = sqrt(endpt_var)))


# plots

par(mfrow = c(1,3))
hist(cf,
     breaks = 20, xlim = c(0,1),
     main = "", xlab = "")
abline(v=0.5, col = "red")
hist(cf_endpt,
     breaks = 30,
     xlim = c(0,1),
     main = "", xlab = "")
abline(v=0.5, col = "red")
hist(cf_endpt_logit,
     breaks = 30,
     xlim = c(0,1),
     main = "", xlab = "")
abline(v=0.5, col = "red")


# stats

sum(cf > 0.6)/n
sum(cf > 0.7)/n
