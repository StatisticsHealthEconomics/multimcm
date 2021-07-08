
# Stan cut-points
# censored data


library(dplyr)
library(glue)
library(purrr)
library(reshape2)
library(ggplot2)

source("R/batch_runTx.R")
source("R/bmcm_joint_stan_stringTx.R")
source("R/prep_stan_params.R")
source("R/prep_stan_dataTx.R")
source("R/prep_shared_paramsTx.R")
source("R/prep_tx_params.R")
source("R/create_stancodeTx.R")
source("R/create_block_codeTx.R")

data("surv_data_cut")

batch_runTx(model_idx = list(os = 3, pfs = 5),
            data = surv_data_cut$`30`,
            cf_idx = 2,
            save_res = TRUE,
            save_name = "_cpt_30m",
            chains = 1,
            warmup = 100,
            iter = 1000,
            thin = 10)

batch_runTx(model_idx = list(os = 1, pfs = 1),
            data = surv_data_cut$`30`,
            cf_idx = 3,
            save_res = TRUE,
            save_name = "_cpt_30m")


##TODO: how to pass other than first argument?
# ll <-
#   lapply(surv_data_cut,
#          FUN = batch_runTx,
#          model_idx = c(1,1),
#          cf_idx = 3,
#          save_res = TRUE)


#########
# plots #
#########

library(survival)

source("R/plot_S_jointTx.R")
source("R/prep_S_dataTx.R")

out <-
  # readRDS("~/R/rstanbmcm/data/independent/cf hier/bg_fixed_hr1/stan_lognormal_lognormal_cpt_12m.Rds")
  # readRDS("~/R/rstanbmcm/data/independent/cf separate/bg_fixed_hr1/stan_lognormal_lognormal_cpt_12m.Rds")
  # readRDS("~/R/rstanbmcm/data/independent/cf separate/bg_fixed_hr1/stan_exp_exp_cpt_12m.Rds")
  readRDS("~/R/rstanbmcm/data/independent/cf hier/bg_fixed_hr1/stan_exp_exp_cpt_12m.Rds")

gg <- plot_S_jointTx(out,
                     annot_cf = FALSE,
                     data = surv_data_cut$`12`)
gg + xlim(0,5)

