
# library(purrr)
#

prep_S_sepTx_data <- function(stan_out) {

  S_stats <- list()

  model_names <- strsplit(stan_out[[1]]@model_name, "_")[[1]][c(1, 2)]

  param_names <- c("S_os", "S_pfs", "S_bg", "S_os_pred", "S_pfs_pred")

  stan_extract <- map(stan_out, rstan::extract)
  stan_extract <- map(stan_extract, function(x) x[param_names])

  dat <- list()
  for (i in param_names) {

    dat[[i]] <- cbind(stan_extract[[1]][[i]],
                      stan_extract[[2]][[i]],
                      stan_extract[[3]][[i]])

    ##TODO: check permutation is correct
    dim(dat[[i]]) <- c(nrow(stan_extract[[1]][[i]]),
                       ncol(stan_extract[[1]][[i]]), 3)
  }

  CI_probs <- c(0.025, 0.5, 0.975)

  S_stats$os <-
    prep_S_endpt_sepTx(dat,
                       event_type = "os")

  S_stats$pfs <-
    prep_S_endpt_sepTx(dat,
                       event_type = "pfs")

  ##TODO: remove hardcoding
  n_tx <- 3

  # unnest
  plot_dat <-
    S_stats %>%
    map(bind_rows, .id = "Tx") %>%
    bind_rows(.id = "event_type") %>%
    mutate(scenario = paste(event_type, Tx, sep = "_"),
           type_tx = paste(type, Tx, sep = "_"),
           Tx = ifelse(type == "S_bg", "background", Tx),
           Tx = factor(Tx)
           # event_type = ifelse(event_type == "os",
           #                     model_names[1], model_names[2])
           )

  plot_dat
}

