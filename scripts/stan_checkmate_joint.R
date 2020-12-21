
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

# test...
out <-
  bmcm_joint_stan_file(
    input_data = surv_input_data,
    model_os = "exp",
    model_pfs = "exp",
    tx_name = "NIVOLUMAB",
    params_pfs = list(mu_0 = c(-3, 0),
                      sigma_0 = c(0.5, 0.01)),
    params_os = list(mu_0 = c(-3, 0),
                     sigma_0 = c(0.4, 1)),
    params_cf = list(mu_cf = -0.8,            # hierarchical cure fraction
                     sigma_cf = 2,
                     sd_cf_os = 0.5,
                     sd_cf_pfs = 0.5),
    cf_model = 2,                # 1- shared; 2- separate; 3- hierarchical
    joint_model = FALSE,
    warmup = 100,
    iter = 1000,
    thin = 10)


# save(stan_files, file = "data/stan_joint_filenames.RData")

# plot_S_event_type(stan_files$exp)

stan_list <- list(NIVOLUMAB = out)
plot_S_joint(stan_list = stan_list)

plot_prior_predictive(out, event_type = "os")
plot_prior_predictive(out, event_type = "pfs")

