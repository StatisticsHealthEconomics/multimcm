
# run Stan mixture cure joint model
# CheckMate 067 data set

library(parallel)

source("R/batch_runTx.R")


data("surv_input_data")

cl <- makeCluster(detectCores() - 1)

clusterEvalQ(cl, {
  library(purrr)
  library(reshape2)
  library(dplyr)
  library(rstan)
  library(dplyr)
  library(glue)
  library(reshape2)

  source("R/prep_stan_params.R")
  source("R/prep_shared_paramsTx.R")
  source("R/prep_stan_dataTx.R")
  source("R/create_stancodeTx.R")
  source("R/create_block_codeTx.R")
  source("R/bmcm_joint_stan_stringTx.R")
  source("R/batch_runTx.R")
  source("R/prep_tx_params.R")
})

clusterExport(cl, "surv_input_data")

model_idx <-
  split(
    expand.grid(1:5, 1:5) %>%
      setNames(c("os","pfs")), 1:25)

ll <-
  parLapply(cl,
            X = model_idx[1:2],
            fun = batch_runTx,
            data = surv_input_data,
            save_res = TRUE)

stopCluster(cl)

