
# run Stan mixture cure joint model
# CheckMate 067 data set
# for all distribution & test combinations
# save output to file


library(purrr)
library(reshape2)
library(dplyr)
library(rstan)
library(shinystan)
library(dplyr)
library(glue)
library(ggplot2)
library(reshape2)


source("R/bmcm_joint_stan_file.R")
source("R/prep_stan_params.R")
source("R/prep_shared_params.R")
source("R/prep_stan_data.R")
source("R/plot_post_pred_KM.R")

source("R/create_stancode.R")
source("R/create_block_code.R")
source("R/bmcm_joint_stan_string.R")
source("R/batch_run.R")


for (trta_idx in 2:2) {
  for (pfs_idx in 5:5) {
    for (os_idx in 1:5) {
      batch_run(os_idx, pfs_idx, trta_idx)
    }
  }
}


