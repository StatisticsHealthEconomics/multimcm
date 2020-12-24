
## fake_data_joint_analysis
## os times
##


# generate fake checkmate data
##TODO: problem that event time are too big
##      and then messes-up the joint regression

s_tx_groups <- table(surv_input_data$TRTA)
mu0 <- -2
OSage_NIVO <- surv_input_data$OSage[surv_input_data$TRTA == "NIVOLUMAB"]

fake_nivo_pfs <-
  rsurv_mix(cf = 0.45,
            n = s_tx_groups["NIVOLUMAB"],
            distn = c("exp", "exp"),
            prop_cens = 0.2,
            params =
              list(
                list(
                  mu = c(mu0, 0.005)),
                list(
                  mu = c(-8.5, 0.05))),
            X = OSage_NIVO)

# centered for joint model
surv_input_fake <-
  surv_input_data %>%
  mutate(pfs = ifelse(TRTA == "NIVOLUMAB",
                      fake_nivo_pfs$t_cens, pfs),
         pfs_event = ifelse(TRTA == "NIVOLUMAB",
                            fake_nivo_pfs$status, pfs_event)) %>%
  group_by(TRTA) %>%
  mutate(pfs_centred = pfs - 1/exp(mu0))

X_nivo <-
  data.frame(
    OSage = OSage_NIVO,
    t_pfs = surv_input_fake$pfs_centred[surv_input_fake$TRTA == "NIVOLUMAB"])

fake_nivo_os <-
  rsurv_mix(cf = 0.2,
            n = s_tx_groups["NIVOLUMAB"],
            distn = c("exp", "exp"),
            prop_cens = 0.2,
            params =
              list(
                list(
                  mu = c(-3, 0.005, -0.001)),
                list(
                  mu = c(-8.5, 0.03, 0))),
            X = X_nivo)

# replace with fake data
surv_input_fake$os[surv_input_fake$TRTA == "NIVOLUMAB"] <- fake_nivo_os$t_cens
surv_input_fake$os_event[surv_input_fake$TRTA == "NIVOLUMAB"] <- fake_nivo_os$status


## plots
# check Kaplan-Meier

library(survival)
fit_pfs <- survfit(Surv(fake_nivo_pfs$t_cens, fake_nivo_pfs$status) ~ fake_nivo_pfs$group)
fit_mix <- survfit(Surv(fake_nivo_pfs$t_cens, fake_nivo_pfs$status) ~  1)
plot(fit_pfs, xlim = c(0, 60))
lines(fit_mix, col = "blue")

fit_os <- survfit(Surv(fake_nivo_os$t_cens, fake_nivo_os$status) ~ fake_nivo_os$group)
fit_mix <- survfit(Surv(fake_nivo_os$t_cens, fake_nivo_os$status) ~  1)
plot(fit_os, xlim = c(0, 60))
lines(fit_mix, col = "blue")


##################
# run stan model #
##################

# cf separate
# event time joint distn
out <-
  bmcm_joint_stan_file(
    input_data = surv_input_fake,
    model_os = "exp",
    model_pfs = "exp",
    tx_name = "NIVOLUMAB",
    params_pfs = list(mu_0 = c(-3, 0),
                      sigma_0 = c(0.5, 0.01)),
    params_os = list(mu_0 = c(-3, 0),
                     sigma_0 = c(0.4, 1)),
    params_cf = list(mu_cf_os = array(-0.8, 1),
                     mu_cf_pfs = array(-0.8, 1),
                     sd_cf_os = array(0.5, 1),
                     sd_cf_pfs = array(0.5, 1)),
    params_joint = list(mu_joint = array(-0.1, 1),
                        sigma_joint = array(1, 1)),
    cf_model = 2,
    joint_model = 1, #TRUE
    warmup = 500,
    iter = 2000,
    thin = 20)

stan_list <- list("NIVOLUMAB" = out)
gg <- plot_S_joint(stan_list = stan_list)
gg

# overlay Kaplan-Meier
library(survival)
fit_os <- survfit(Surv(os, os_event) ~ 1,
                  data = filter(surv_input_fake, TRTA == trta))
fit_pfs <- survfit(Surv(pfs, pfs_event) ~ 1,
                   data = filter(surv_input_fake, TRTA == trta))
km_data <-
  rbind(
    data.frame(Tx = trta,
               event_type = "os",
               time = fit_os$time,
               surv = fit_os$surv),
    data.frame(Tx = trta,
               event_type = "pfs",
               time = fit_pfs$time,
               surv = fit_pfs$surv))

gg + geom_line(aes(x = time, y = surv),
               data = km_data,
               lwd = 1,
               inherit.aes = FALSE) +
  xlim(0, 60)


