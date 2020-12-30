
# stand-alone event time posterior predictions
# Kaplan-Meier plots
#


##TODO: can we avoid looping over posterior samples?
#https://mc-stan.org/docs/2_23/stan-users-guide/stand-alone-generated-quantities-and-ongoing-prediction.html
# res <-
#   rstan::stan(
#     file = here::here("inst/stan/postpred_exp_exp.stan"),
#     data = list(n = 311))


## stan output
# stan_out <- readRDS("~/R/rstanbmcm/data/cf separate/stan_exp_exp_IPI_.Rds")
# stan_out <- readRDS("~/R/rstanbmcm/data/cf separate/stan_exp_exp_NIVOLUMAB_.Rds")
stan_out <- readRDS("~/R/rstanbmcm/data/cf separate/stan_exp_exp_NIVOLUMAB+IPILIMUMAB_.Rds")
xx <- rstan::extract(stan_out)

# explicitly looping over samples

# # using lambda means
# res <-
#   rstan::stan(
#     file = here::here("inst/stan/postpred_exp_exp.stan"),
#     data = list(n = ncol(xx$lp_os),
#                 n_samples = nrow(xx$beta_os),
#                 curefrac = xx$cf_os,
#                 beta_os= xx$beta_os,
#                 beta_bg = xx$beta_bg),
#     chains = 1,
#     warmup = 0,
#     iter = 1,
#     algorithm = "Fixed_param")


# using lambda case-mix
res <-
  rstan::stan(
    file = here::here("inst/stan/postpred_exp_exp.stan"),
    data = list(n = ncol(xx$lp_os),
                n_samples = nrow(xx$beta_os),
                cf_os = xx$cf_os,
                cf_pfs = xx$cf_pfs,
                lambda_os = xx$lambda_os,
                lambda_os_bg = xx$lambda_os_bg,
                lambda_pfs = xx$lambda_pfs,
                lambda_pfs_bg = xx$lambda_pfs_bg),
    chains = 1,
    warmup = 0,
    iter = 1,
    algorithm = "Fixed_param")


######################
# Kaplan-Meier plots #
######################

library(survival)

# tx_name <- "IPILIMUMAB"
# tx_name <- "NIVOLUMAB"
tx_name <- "NIVOLUMAB+IPILIMUMAB"
n_post <- 90

load("~/R/rstanbmcm/data/surv_input_data.RData")
real_data <- surv_input_data[surv_input_data$TRTA == tx_name, ]

yy <- rstan::extract(res)

png(filename = paste0("plots/post_pred_cfsep_exp_exp_", tx_name, ".png"))
par(mfrow = c(1,2))

# os
y_tilde <- yy$t_os_tilde[1, 1, ]
fit <- survfit(Surv(y_tilde, rep(1, length(y_tilde))) ~ 1)
plot(fit, col = "lightblue", xlim = c(0, 60), conf.int = FALSE, main = "OS", ylab = "Survival", xlab = "Month", bty = "n")
for (i in 2:n_post) {
  fit <- survfit(Surv(yy$t_os_tilde[1, i, ], rep(1, length(y_tilde))) ~ 1)
  lines(fit, col = "lightblue", conf.int = FALSE)
}

fit <- survfit(Surv(real_data$os, real_data$os_event) ~ 1)
lines(fit, lwd = 2.5, conf.int = FALSE)

# pfs
y_tilde <- yy$t_pfs_tilde[1, 1, ]
fit <- survfit(Surv(y_tilde, rep(1, length(y_tilde))) ~ 1)
plot(fit, col = "lightblue", xlim = c(0, 60), conf.int = FALSE, main = "PFS", xlab = "Month", bty = "n")

for (i in 2:n_post) {
  fit <- survfit(Surv(yy$t_pfs_tilde[1, i, ], rep(1, length(y_tilde))) ~ 1)
  lines(fit, col = "lightblue", conf.int = FALSE)
}

fit <- survfit(Surv(real_data$pfs, real_data$pfs_event) ~ 1)
lines(fit, lwd = 2.5, conf.int = FALSE)

title(tx_name, line = -1, outer = TRUE)
dev.off()
