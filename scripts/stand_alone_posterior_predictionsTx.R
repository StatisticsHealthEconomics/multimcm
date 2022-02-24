
# stand-alone event time posterior predictions
# all treatment model
# Kaplan-Meier plots


library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)

###################
# select analysis #
###################

source("R/define_setup.R")
source("R/create_stancode_postpredTx.R")
source("R/post_pred_block_code.R")

load("~/R/rstanbmcm/data/surv_input_data.RData")

inp <-
  define_setup(
    trta = NA,
    joint_model = "independent",
    cf_idx = 3,
    bg_model_idx = 2,
    model_os_idx = 2,
    model_pfs_idx = 1)

stan_out <-
  readRDS(here::here(
    glue::glue_data(inp, "data/{joint_model}/{cf_model}/{bg_model}/stan_{model_os}_{model_pfs}.Rds")))

dat <- rstan::extract(stan_out)

stancode_ppv <-
  create_stancode_postpredTx(os_model = inp$model_os,
                             pfs_model = inp$model_pfs)

# repeat cure fraction values
# so in same expanded wide format as other variables
nTx <- table(surv_input_data$TRTA, exclude = "")
cf_os <- dat$cf_os[ , rep(c(1,2,3), times = nTx)]
cf_pfs <- dat$cf_pfs[ , rep(c(1,2,3), times = nTx)]


##########
# sample #
##########

# subset posterior samples
ns <- 20

# explicitly looping over samples
# using lambda case-mix or means
res <-
  rstan::stan(
    # file = here::here("inst/stan/postpred_exp_expTx.stan"),
    model_code = stancode_ppv,
    data = list(N_os = ncol(cf_os),
                N_pfs = ncol(cf_pfs),
                os_model = 1,
                pfs_model = 1,
                n_samples = ns,
                cf_os = cf_os[1:ns, ],
                cf_pfs = cf_pfs[1:ns, ],
                shape_os = dat$shape_os[1:ns],
                shape_pfs = dat$shape_pfs[1:ns],
                lambda_os = dat$lambda_os[1:ns, ],
                lambda_pfs = dat$lambda_pfs[1:ns, ],
                lambda_os_bg = dat$lambda_os_bg[1:ns, ],
                lambda_pfs_bg = dat$lambda_pfs_bg[1:ns, ]),
    chains = 1,
    warmup = 0,
    iter = 1,
    algorithm = "Fixed_param")

stan_extract <-
  rstan::extract(res) %>%
  lapply(drop)

################
# Kaplan-Meier #
################

library(survival)
library(dplyr)
library(reshape2)

source("R/plot_post_pred_KM.R")
source("R/geom_kaplan_meier.R")

plot_post_pred_Tx(res, surv_input_data,
                  casemix = TRUE,
                  event_type = "pfs") #+
  # xlim(0,20)

