
# posterior survival statistics
#
# restricted mean survival times
# and median survival times


library(dplyr)
library(reshape2)
library(purrr)
library(ggplot2)

source("R/prep_S_dataTx.R")

stan_exp_exp <-
  readRDS(here::here("data", "independent", "cf hier", "bg_fixed_hr1", "stan_exp_exp.Rds"))

stan_lnorm_lnorm <-
  readRDS(here::here("data", "independent", "cf hier", "bg_fixed_hr1", "stan_lognormal_lognormal.Rds"))

# plot_dat <- prep_S_jointTx_data(stan_exp_exp)
plot_dat <- prep_S_jointTx_data(stan_lnorm_lnorm)

# restricted mean survival times
rmst <-
  plot_dat %>%
  group_by(event_type, Tx, month) %>%
  # remove duplicates
  distinct(mean, .keep_all = TRUE) %>%
  group_by(event_type, Tx) %>%
  summarise(rmst_mean = sum(mean),
            rmst_low = sum(lower),
            rmst_upp = sum(upper))

medians <-
  plot_dat %>%
  group_by(event_type, Tx, month) %>%
  # remove duplicates
  distinct(mean, .keep_all = TRUE) %>%
  group_by(event_type, Tx) %>%
  filter(mean < 0.5) %>%
  arrange(month) %>%
  filter(row_number() == 1)
