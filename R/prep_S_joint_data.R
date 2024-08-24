
#' Prepare data for survival plot
#'
#' @param bmcm_out Output of Stan model
#' @return data frame
#' @importFrom glue glue
#'
prep_S_joint_data <- function(bmcm_out) {

  S_stats <- list()
  n_groups <- bmcm_out$formula$cure$n_groups

  event_type <- 1:n_groups
  model_names <- bmcm_out$distns
  n_tx <- bmcm_out$formula$cure$fe_nlevels[1]

  stan_extract <- stan_extract(bmcm_out)

  CI_probs <- c(0.025, 0.5, 0.975)

  S_stats <- list()
  # summary statistics for each end point
  for (i in seq_len(n_groups)) {

    S_stats[[i]] <-
      prep_S_data(stan_extract,
                  event_type = i)

    ##TODO:
    # label[[i]] <-
    #   apply(X = stan_extract[[glue("cf_{i}")]], 2,
    #         FUN = function(x)
    #           paste(round(quantile(x, probs = CI_probs), 2),
    #                 collapse = " "))
  }

  ann_text <-
    data.frame(
      event_type = model_names,
      Tx = rep(1:n_tx, each = n_groups))#,
  # label = label)

  # unnest
  plot_dat <-
    S_stats %>%
    map(bind_rows, .id = "Tx") %>%
    bind_rows(.id = "event_type") %>%
    mutate(scenario = paste(event_type, Tx, sep = "_"),
           type_tx = paste(type, Tx, sep = "_"),
           Tx = ifelse(type == "S_bg", "background",
                       ifelse(grepl("^S_\\d+$", type),
                              "uncured", Tx)),
           Tx = factor(Tx),
           # endpoint = factor(toupper(event_type)) ##TODO: distinguish between numeric and strings
           endpoint = as.numeric(event_type)
           # model_name = as.factor(model_names)  ##TODO:
    ) %>%
    arrange(endpoint)

  plot_dat
}


#
stan_extract <- function(bmcm_out, pattern = "") {
  fit <- bmcm_out$output

  if (inherits(fit, "stanfit")) {

    samples <- rstan::extract(fit)
    param_names <- grep(pattern, names(samples), value = TRUE)
    extracted_params <- samples[param_names]

  } else if (inherits(fit, "CmdStanMCMC")) {

    samples <- fit$draws(format = "df")
    param_names <- grep(pattern, names(samples), value = TRUE)
    extracted_params <- samples[, param_names]
  } else {
    stop("Fit object must be of class 'stanfit' (rstan) or 'CmdStanMCMC' (cmdstanr).")
  }

  extracted_params
}

