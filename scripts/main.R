
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

# rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)

data("surv_input_data")


#################
# model set-up

save_res <- TRUE
TRTX <- NA

# # types of model
# latent_formula = "Surv(time=month, event=status) ~ 1 + age_event",
# cure_formula = "~ TRTA + event_idx",                                     # separate
# cure_formula = "~ TRTA",                                                 # pooled
jk

##############
# prep data

# rearrange data in to long format so can have
# arbitrary number of end types (not just os, pfs)

long_event_time_dat <-
  surv_input_data |>
  select(AAGE, os, pfs, TRTA, SEX, ACOUNTRY, OS_rate, PFS_rate) |>
  mutate(id = 1:n()) |>
  melt(measure.vars = c("os", "pfs"),
       value.name = "month", variable.name = "event_name") |>
  mutate(year = floor(month/12),
         age_event = AAGE + year,
         bg_rate = ifelse(event_name == "os", OS_rate, PFS_rate),
         bg_rate = ifelse(bg_rate == 0, 0.00001, bg_rate)) |>  # replace so >0
  select(-OS_rate, -PFS_rate)

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
    formula = "Surv(time=month, event=status) ~ 1 + age_event",
    cureformula = "~ TRTA + (1 | event_idx)",    # hierarchical
    family_latent = "exponential",
    prior_latent = NA,
    prior_cure = NA,
    centre_coefs = TRUE,
    bg_model = "bg_fixed",
    bg_hr = 1,
    t_max = 60)

if (save_res) {saveRDS(out, file = "data/stan_res.RData")}


##########
# plots

gg <- plot_S_joint(out$stan_output,
                   annot_cf = FALSE,
                   data = surv_input_data)
gg

# ggsave(gg, filename)

