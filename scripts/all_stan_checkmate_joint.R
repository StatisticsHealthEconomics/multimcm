# run Stan mixture cure joint model
# CheckMate 067 data set


library(purrr)
library(reshape2)
library(dplyr)
library(rstan)
library(shinystan)
library(dplyr)
library(ggplot2)

# library(rstanbmcm)
# devtools::load_all()

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)


## surv_input_data
# load("~/Documents/R/mixture_cure_model/data/surv_input_data.RData")
data("surv_input_data")

##TODO: move this to data prep script in surv_input_data package
surv_input_data$csex <-
  as.numeric(as.factor(surv_input_data$SEX)) - 1.5

all_tx_names <- c("IPILIMUMAB", "NIVOLUMAB", "NIVOLUMAB+IPILIMUMAB")
model_names <- c("exp", "weibull", "gompertz")

## choose compiled stan?
stan_fn <- bmcm_joint_stan_file
# stan_fn <- bmcm_joint_stan

stan_files <- list()

# k <- model_names[1]
# i <- model_names[1]
# j <- all_tx_names[1]

for (k in model_names) {
  for (i in model_names) {
    for (j in all_tx_names) {
      # tryCatch({

      out <- stan_fn(input_data = surv_input_data,
                     model_os = k,
                     model_pfs = i,
                     tx_name = j,
                     warmup = 1,#000,
                     iter = 100,#00,
                     thin = 1)#0)

      file_name <-
        here::here(paste("data/stan", k, i, j, ".Rds", sep = "_"))

      stan_files[[k]][[i]][[j]] <- file_name
      saveRDS(out, file = file_name)

      # },
      # error = function(e) e)
    }
  }
}

