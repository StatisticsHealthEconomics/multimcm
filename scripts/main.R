
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

# distn <- c("lognormal", "lognormal")
distn <- "exponential"


##############
# prep data

# rearrange data in to long format so can have
# arbitrary number of end types (not just os, pfs)

input_data <-
  surv_input_data |>
  filter(if (!is.na(TRTX)) TRTA == TRTX else TRTA != "") |>  # remove empty treatments
  mutate(id = 1:n(),
         TRTA_id = as.numeric(as.factor(TRTA))) |>
  group_by(TRTA) |>
  mutate(tx_id = 1:n()) |>
  ungroup()


# melt by time of event
long_event_time_dat <-
  input_data |>
  select(id, TRTA_id, tx_id, AAGE, os, pfs, TRTA,
         SEX, ACOUNTRY, OS_rate, PFS_rate) |>
  melt(measure.vars = c("os", "pfs"),
       value.name = "month",
       variable.name = "event_name") |>
  mutate(year = floor(month/12),
         event_idx = as.numeric(as.factor(event_name)),
         age_event = AAGE + year,
         bg_rate = ifelse(event_name == "os", OS_rate, PFS_rate),
         bg_rate = bg_rate/12,                                 # convert to months
         bg_rate = ifelse(bg_rate == 0, 0.00001, bg_rate)) |>  # replace so >0
  select(-OS_rate, -PFS_rate)

# melt by censoring indicator
long_input_data <-
  input_data |>
  select(id, os_event, pfs_event) |>
  rename(os = os_event, pfs = pfs_event) |>
  melt(measure.vars = c("os", "pfs"),
       value.name = "status",
       variable.name = "event_name") |>
  merge(long_event_time_dat, by = c("id", "event_name")) |>
  mutate(event_idx = ifelse(event_name == "os", 1, 2),
         TRTA = as.factor(TRTA)) |>
  arrange(id)

# save(long_input_data, file = "data/long_input_data.RData")


##############
# run model

out <-
  bmcm_stan(
    input_data = long_input_data,
    formula = "Surv(time=month, event=status) ~ 1 + age_event",
    cureformula = "~ TRTA + (1 | event_idx)",    # hierarchical
    family_latent = distn,
    prior_latent = NA,
    prior_cure = NA,
    centre_coefs = TRUE,
    bg_model = "bg_fixed",
    bg_hr = 1,
    t_max = 60)

if (save_res) {
  save(out, file = glue::glue("data/{out$output@model_name}.Rds"))}


##########
# plots

gg <- plot_S_joint(out, add_km = TRUE,
                   annot_cf = FALSE)
gg

# ggsave(gg, filename = glue::glue("plots/{out$output@model_name}_S_plot.png")))

