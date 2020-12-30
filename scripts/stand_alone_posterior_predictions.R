
# stand-alone event time posterior predictions
# Kaplan-Meier plots
#


##TODO: can we avoid looping over posterior samples?
#https://mc-stan.org/docs/2_23/stan-users-guide/stand-alone-generated-quantities-and-ongoing-prediction.html
# res <-
#   rstan::stan(
#     file = here::here("inst/stan/postpred_exp_exp.stan"),
#     data = list(n = 311))


# explicitly looping over samples

stan_out <- readRDS("~/R/rstanbmcm/data/cf separate/stan_exp_exp_IPI_.Rds")
xx <- extract(stan_out)

res <-
  stan(file = here::here("inst/stan/postpred_exp_exp.stan"),
       data = list(n = ncol(xx$lp_os),
                   n_samples = nrow(xx$beta_os),
                   curefrac = xx$cf_os,
                   beta_os= xx$beta_os,
                   beta_bg = xx$beta_bg),
       chains = 1,
       warmup = 0,
       iter = 1,
       algorithm = "Fixed_param")


library(survival)
yy <- extract(res)
fit <- survfit(Surv(yy$t_tilde[1, 1, ], rep(1, 311)) ~ 1)
plot(fit, col = "lightblue", xlim = c(0, 60), conf.int = FALSE)

for (i in 2:90) {
  fit <- survfit(Surv(yy$t_tilde[1, i, ], rep(1, 311)) ~ 1)
  lines(fit, col = "lightblue", conf.int = FALSE)
}

real_data <- surv_input_data[surv_input_data$TRTA == "IPILIMUMAB", ]
fit <- survfit(Surv(real_data$os, real_data$os_event) ~ 1)
lines(fit, lwd = 2.5, conf.int = FALSE)

