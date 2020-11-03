
# run stan mixture cure joint model
# CheckMate 067 dataset


library(purrr)
library(reshape2)
library(dplyr)
library(rstan)
library(shinystan)
library(dplyr)
library(ggplot2)
# library(rstanbmcm)
devtools::load_all()

## surv_input_data
# load("C:/Users/Nathan/Documents/R/mixture_cure_model/data/surv_input_data.RData")
data("surv_input_data")

all_tx_names <- c("IPILIMUMAB", "NIVOLUMAB", "NIVOLUMAB+IPILIMUMAB")
all_event_types <- c("PFS", "OS")
model_names <- c("exp", "weibull")#, "gompertz")
# model_names <- c("exp_full", "weibull_full", "gompertz_full") # age-dependent cure fraction

## choose compiled stan?
# stan_fn <- bmcm_joint_stan_file
stan_fn <- bmcm_joint_stan

stan_files <- list()

for (k in model_names) {
  for (i in model_names) {
    for (j in all_tx_names) {
      # tryCatch({

      out <- stan_fn(input_data = surv_input_data,
                     model_os = k,
                     model_pfs = i,
                     tx_name = j,
                     warmup = 1000,
                     iter = 10000,
                     thin = 10)

      file_name <-
        here::here(paste("data/stan", k, i, j, ".Rds", sep = "_"))

      stan_files[[k]][[i]][[j]] <- file_name
      saveRDS(out, file = file_name)

      # },
      # error = function(e) e)
    }
  }
}

# save(stan_files, file = "data/stan_joint_filenames.RData")

plot_S_event_type(stan_files$exp)
plot_S_event_type(stan_files$weibull)

