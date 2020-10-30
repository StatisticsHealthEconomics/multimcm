
# run stan mixture cure model
# CheckMate 067 dataset


# kill chains with:
# system("killall R")
# system("taskkill /F /IM R.exe /T")

library(purrr)
library(reshape2)
library(dplyr)
library(rstan)
library(shinystan)
library(dplyr)
library(ggplot2)
# library(rstanbmcm)
devtools::load_all()

# surv_input_data
# load("C:/Users/Nathan/Documents/R/mixture_cure_model/data/surv_input_data.RData")
data("surv_input_data")

all_tx_names <- c("IPILIMUMAB", "NIVOLUMAB", "NIVOLUMAB+IPILIMUMAB")
all_event_types <- c("PFS", "OS")
model_names <- c("exp", "weibull")#, "gompertz")
# model_names <- c("exp_full", "weibull_full", "gompertz_full") # age-dependent cure fraction

## choose compiled stan?
# stan_fn <- bmcm_stan_file
stan_fn <- bmcm_stan

stan_files <- list()

for (k in model_names) {
  for (i in all_event_types) {
    for (j in all_tx_names) {
      # tryCatch({

      out <- stan_fn(input_data = surv_input_data,
                     model = k,
                     event_type = i,
                     tx_name = j,
                     warmup = 1000,
                     iter = 10000,#50000,
                     thin = 10)#50)

      file_name <-
        here::here(paste("data/stan", k, i, j, ".Rds", sep = "_"))

      stan_files[[k]][[i]][[j]] <- file_name
      saveRDS(out, file = file_name)

      # },
      # error = function(e) e)
    }
  }
}

# save(stan_files, file = "data/stan_filenames.RData")

plot_S_event_type(stan_files$exp)
plot_S_event_type(stan_files$weibull)


####################
# 40% cure fraction
####################

stan_files_40 <- list()

for (k in model_names) {
  for (i in all_event_types) {
    for (j in all_tx_names) {
      # tryCatch({

      out_40 <- stan_fn(input_data = surv_input_data,
                        model = k,
                        event_type = i,
                        tx_name = j,
                        mean_cf = 0.4,
                        var_cf = 0.1,
                        warmup = 10,
                        iter = 500,
                        thin = 1)

      file_name <-
        here::here(paste("data/stan_40", k, i, j, ".Rds", sep = "_"))

      stan_files_40[[k]][[i]][[j]] <- file_name
      saveRDS(out_40, file = file_name)

      # },
      # error = function(e) e)
    }
  }
}

# save(stan_files_40, file = "data/stan_40_filenames.RData")

plot_S_event_type(stan_files_40$exp)
plot_S_event_type(stan_files_40$weibull)


####################
# 10% cure fraction
####################




# -------------------------------------------------------------------------

# priors for cure fraction
## logit
beta_nivo <- c(-0.2, 0.001) # mean 45%
beta_ipi <- c(-1.2, 0.001)  # mean 23%
beta_both <- c(0.16, 0.001) # mean 54%

## beta
mean_cf <- 0.45
var_cf <- 0.001
