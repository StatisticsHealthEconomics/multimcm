
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
library(abind)

# library(rstanbmcm)
# devtools::load_all()

## reading these in for now since building the package
## takes time to compile the Stan code
source("R/prep_stan_params.R")
source("R/prep_shared_params.R")
source("R/prep_stan_data.R")
source("R/prep_tx_params.R")
source("R/bmcm_stan.R")
source("R/create_stancode.R")
source("R/create_block_code.R")
source("R/parse_formula.R")


# rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)

data("surv_input_data")


#################
# model set-up

## how many treatments?
TRTX <- NA  # all treatments
# TRTX <- "IPILIMUMAB"  # single treatment only

save_res <- TRUE

model_names <- c("exp", "weibull", "gompertz", "loglogistic", "lognormal")
model_os_idx <- 1
model_pfs_idx <- 1

cf_model_names <- c("cf pooled", "cf separate", "cf hier")
cf_idx <- 3

bg_model_names <- c("bg_distn", "bg_fixed")
bg_model_idx <- 2

# background hazard ratio
bg_hr <- 1


##############
# prep data

model_os <- model_names[model_os_idx]
model_pfs <- model_names[model_pfs_idx]
bg_model <- bg_model_names[bg_model_idx]

##TODO: do we need rates?


# rearrange data in to long format so can have
# arbitrary number of end types (not just os, pfs)

long_event_time_dat <-
  surv_input_data |>
  select(AAGE, os, pfs, TRTA, SEX, ACOUNTRY) |>
  mutate(id = 1:n()) |>
  melt(measure.vars = c("os", "pfs"),
       value.name = "month", variable.name = "event") |>
  mutate(year = floor(month/12),
         age_event = AAGE + year)

long_input_data <-
  surv_input_data |>
  select(os_event, pfs_event) |>
  rename(os = os_event, pfs = pfs_event) |>
  mutate(id = 1:n()) |>
  melt(measure.vars = c("os", "pfs"),
       value.name = "status", variable.name = "event") |>
  merge(long_event_time_dat) |>
  filter(if (!is.na(TRTX)) TRTA == TRTX else TRTA != "")


## prior hyper-parameters

# all treatments
params_cf_lup <-
  list("cf pooled" = NULL,
       "cf separate" = NULL,
       "cf hier" =
         list(mu_sd_cf = c(0, 0, 0),
              sigma_sd_cf = c(2.5, 2.5, 2.5)))

params_cf <-
  if (is.null(params_cf_lup[[cf_idx]][[model_pfs]])) {
    params_cf_lup[[cf_idx]]
  } else {
    params_cf_lup[[cf_idx]][[model_pfs]]
  }

# same for all tx
mu_alpha <- c(-0.6, -0.6, -0.6)
sigma_alpha <- c(0.8, 0.8, 0.8)

params_tx <-
  if (cf_idx == 1) {
    list(mu_alpha = mu_alpha,
         sigma_alpha = sigma_alpha)
  } else if (cf_idx == 2) {
    list(mu_alpha_os = mu_alpha,
         sigma_alpha_os = sigma_alpha,
         mu_alpha_pfs = mu_alpha,
         sigma_alpha_pfs = sigma_alpha)
  } else if (cf_idx == 3) {
    list(mu_alpha = mu_alpha,
         sigma_alpha = sigma_alpha)
  } else {
    NA
  }


##############
# run model

out <-
  bmcm_stan(
    input_data = long_input_data,
    formula = "Surv(months, status) ~ 1 + (1|event) + TRTA",
    distns = "exp",
    params_cf = c(params_cf, params_tx),
    cf_model = "cf hier",
    joint_model = FALSE,
    bg_model = bg_model_idx,
    bg_hr = bg_hr,
    t_max = 60,
    chains = 1,
    warmup = 100,
    iter = 500,
    thin = 1)

# if (save_res) {saveRDS(out, file)}


##########
# plots

library(survival)

source("R/plot_S_jointTx.R")
source("R/prep_S_data.R")
source("R/prep_S_jointTx_data.R")
source("R/geom_kaplan_meier.R")

gg <- plot_S_jointTx(out,
                     annot_cf = FALSE,
                     data = surv_input_data)
gg

# ggsave(gg, filename)

