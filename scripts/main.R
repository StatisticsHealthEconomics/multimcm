
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
library(survival)
# library(rstanarm)

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

model_names <- c("exponential", "weibull", "gompertz", "loglogistic", "lognormal")

# latent_formula = "Surv(time=month, event=status) ~ 1 + age_event",
# cure_formula = "~ TRTA + event_idx",                                     # separate
#
# latent_formula = "Surv(time=month, event=status) ~ 1 + age_event + event_idx",
# cure_formula = "~ TRTA",                                                 # pooled

bg_model_names <- c("bg_distn", "bg_fixed")
bg_model_idx <- 2

# background hazard ratio
bg_hr <- 1


##############
# prep data

##TODO: do we need rates?
# mutate(rate = ifelse(rate == 0, 0.00001, rate)) # replace so >0


# rearrange data in to long format so can have
# arbitrary number of end types (not just os, pfs)

long_event_time_dat <-
  surv_input_data |>
  select(AAGE, os, pfs, TRTA, SEX, ACOUNTRY) |>
  mutate(id = 1:n()) |>
  melt(measure.vars = c("os", "pfs"),
       value.name = "month", variable.name = "event_name") |>
  mutate(year = floor(month/12),
         age_event = AAGE + year)

long_input_data <-
  surv_input_data |>
  select(os_event, pfs_event) |>
  rename(os = os_event, pfs = pfs_event) |>
  mutate(id = 1:n()) |>
  melt(measure.vars = c("os", "pfs"),
       value.name = "status", variable.name = "event_name") |>
  merge(long_event_time_dat) |>
  filter(if (!is.na(TRTX)) TRTA == TRTX else TRTA != "") |>
  mutate(event_idx = ifelse(event_name == "os", 1, 2))


##############
# run model

out <-
  bmcm_stan(
    input_data = long_input_data,
    formula = "Surv(time=month, event=status) ~ 1 + age_event",  # hierarchical
    cureformula = "~ TRTA + (1 | event_idx)",
    distns = "exponential",
    joint_model = FALSE,
    bg_model = "bg_fixed",
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

