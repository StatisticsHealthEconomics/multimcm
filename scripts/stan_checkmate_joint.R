
# run Stan mixture cure joint model
# CheckMate 067 data set


library(purrr)
library(reshape2)
library(dplyr)
library(rstan)
library(shinystan)
library(dplyr)
library(glue)
library(ggplot2)

# library(rstanbmcm)
# devtools::load_all()
source("R/bmcm_joint_stan_file.R")
source("R/prep_stan_params.R")
source("R/prep_shared_params.R")
source("R/prep_stan_data.R")
source("R/plot_post_pred_KM.R")

source("R/create_stancode.R")
source("R/create_block_code.R")
source("R/bmcm_joint_stan_string.R")


# rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)

# load("~/Documents/R/mixture_cure_model/data/surv_input_data.RData")
data("surv_input_data")


###############
# model setup #
###############

surv_input_data$PFS_rate <- surv_input_data$PFS_rate/12  # months
surv_input_data$OS_rate <- surv_input_data$OS_rate/12

save_res <- TRUE

trta_idx <- 3
all_tx_names <- c("IPILIMUMAB", "NIVOLUMAB", "NIVOLUMAB+IPILIMUMAB")
trta <- all_tx_names[trta_idx]

model_os_idx <- 3
model_pfs_idx <- 3
model_names <- c("exp", "weibull", "gompertz", "loglogistic", "lognormal")#, "gengamma")
model_os <- model_names[model_os_idx]
model_pfs <- model_names[model_pfs_idx]

cf_idx <- 2
cf_model_names <- c("cf pooled", "cf separate", "cf hier")

cf_hier <-
  list(mu_cf_gl = array(-0.8, 1),
       sigma_cf_gl = array(2, 1),
       sd_cf_os = array(0.5, 1),
       sd_cf_pfs = array(0.5, 1))

params_cf <-
  list("cf pooled" =
         list(mu_cf_gl = array(-0.8, 1),
              sigma_cf_gl = array(2, 1)),
       "cf separate" =
         list(mu_cf_os = array(-0.8, 1),
              mu_cf_pfs = array(-0.8, 1),
              sd_cf_os = array(0.5, 1),
              sd_cf_pfs = array(0.5, 1)),
       "cf hier" =
         list(exp = cf_hier,
              weibull = cf_hier,
              gompertz = cf_hier,
              loglogistic = cf_hier,
              gengamma = cf_hier,
              lognormal =
                list(mu_cf_gl = array(-1.8, 1),
                     sigma_cf_gl = array(1, 1),
                     sd_cf_os = array(0.5, 1),
                     sd_cf_pfs = array(0.5, 1))))

bg_model_idx <- 2
bg_model_names <- c("bg_distn", "bg_fixed")
bg_model <- bg_model_names[bg_model_idx]

params_pfs <-
  list(exp =
         list(mu_0 = c(-3, 0),
              sigma_0 = c(0.5, 0.01)),
       loglogistic =
         list(a_shape = 1,
              b_shape = 1,
              mu_0 = c(3, 0),
              sigma_0 = c(0.5, 0.01)),
       weibull =
         list(a_shape = 1,
              b_shape = 1,
              mu_0 = c(3, 0),
              sigma_0 = c(0.5, 0.01)),
       gompertz =
         list(a_shape = 1,
              b_shape = 1000,
              mu_0 = c(-3, 0),
              sigma_0 = c(0.5, 0.01)),
       lognormal =
         list(a_sd = 1,
              b_sd = 2,
              mu_0 = c(1.5, 0),
              sigma_0 = c(0.5, 0.01)),
       gengamma =
         list(a_mu = 1,
              b_mu = 1,
              a_Q = 2,
              b_Q = 1,
              mu_0 = c(-3, 0),
              sigma_0 = c(0.5, 0.01)))

params_os <-
  list(exp =
         list(mu_0 = c(-3, 0),
              sigma_0 = c(0.4, 1)),
       loglogistic =
         list(a_shape = 1,
              b_shape = 1,
              mu_0 = c(3, 0),
              sigma_0 = c(0.4, 1)),
       weibull =
         list(a_shape = 1,
              b_shape = 1,
              mu_0 = c(3, 0),
              sigma_0 = c(0.4, 1)),
       gompertz =
         list(a_shape = 1,
              b_shape = 1000,
              mu_0 = c(-3, 0),
              sigma_0 = c(0.5, 1)),
       lognormal =
         list(a_sd = 2,
              b_sd = 1,
              mu_0 = c(2.5, 0),
              sigma_0 = c(0.5, 0.01)),
       gengamma =
         list(a_mu = 1,
              b_mu = 1,
              a_Q = 2,
              b_Q = 1,
              mu_0 = c(-3, 0),
              sigma_0 = c(0.5, 0.01)))

#######
# run #
#######

out <-
  # bmcm_joint_stan_file(
  bmcm_joint_stan_string(
    input_data = surv_input_data,
    model_os = model_os,
    model_pfs = model_pfs,
    tx_name = trta,
    params_pfs = params_pfs[[model_pfs]],
    params_os = params_os[[model_os]],
    params_cf =
      if (is.null(params_cf[[cf_idx]][[model_pfs]])) {
        params_cf[[cf_idx]]
      } else {
        params_cf[[cf_idx]][[model_pfs]]},
    cf_model = cf_idx,                # 1- shared; 2- separate; 3- hierarchical
    joint_model = FALSE,
    bg_model = bg_model_idx,
    warmup = 100,
    iter = 1000,
    thin = 10)


if (save_res) {
  saveRDS(out,
          file = glue::glue(
            "data/independent/{cf_model_names[cf_idx]}/{bg_model}/stan_{model_os}_{model_pfs}_{trta}.Rds"))}

stan_list <- list(out) %>% setNames(trta)


#########
# plots #
#########

library(survival)

source("R/plot_S_joint.R")
source("R/prep_S_data.R")
source("R/plot_prior_predictions.R")

gg <- plot_S_joint(stan_list = stan_list)

# overlay Kaplan-Meier
##TODO: move to function

fit_os <- survfit(Surv(os, os_event) ~ 1,
                  data = filter(surv_input_data, TRTA == trta))
fit_pfs <- survfit(Surv(pfs, pfs_event) ~ 1,
                   data = filter(surv_input_data, TRTA == trta))
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

s_plot <-
  gg +
  geom_line(aes(x = time, y = surv),
            data = km_data,
            lwd = 1,
            inherit.aes = FALSE) +
  xlim(0, 60)
s_plot

ggsave(s_plot,
       filename = glue::glue(
         "plots/S_plots_{model_os}_{model_pfs}_{cf_model_names[cf_idx]}_{bg_model}_{trta}.png"))


plot_prior_predictive(out, event_type = "os")
plot_prior_predictive(out, event_type = "pfs")


## posterior predictive checks
##TODO: for all distns
# fileloc_out <- glue::glue("plots/post_pred_{model_os}_{model_pfs}_{cf_model_names[cf_idx]}_{bg_model}_{trta}.png")
# plot_post_pred_KM(out, trta, surv_input_data)
# plot_post_pred_KM(out, trta, surv_input_data, fileloc_out)


# overlayed os, pfs plot

gg <- plot_S_joint(stan_list = stan_list, facet = FALSE, annot_cf = FALSE)

s_plot <-
  gg +
  geom_line(aes(x = time, y = surv),
            data = km_data[km_data$event_type == "pfs", ],
            lwd = 1,
            inherit.aes = FALSE) +
  geom_line(aes(x = time, y = surv),
            data = km_data[km_data$event_type == "os", ],
            lwd = 1,
            inherit.aes = FALSE) +
  xlim(0, 60)
s_plot

ggsave(s_plot,
       filename = glue::glue(
         "plots/S_plot_{model_os}_{model_pfs}_{cf_model_names[cf_idx]}_{bg_model}_{trta}.png"))

