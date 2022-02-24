# posterior predictive value survival curve functions

#' Plot posterior predictive Kaplan-Meier.
#' Stan data from separate treatment models (old version).
#'
#' Base R used.
#' stand-alone generated values as input.
#'
#' @param res Stan output including predicted times
#' @param tx_name "IPILIMUMAB", "NIVOLUMAB", "NIVOLUMAB+IPILIMUMAB"
#' @param orig_data Original study data
#' @param fileloc_out File address for output plot with extension
#' @param casemix Use case-mix rates or overall mean
#'
#' @return
#'
#' @import survival dplyr
#' @export
#'
#' @examples
#'
#' tx_name <- "IPILIMUMAB"
#' orig_data <- load("~/R/rstanbmcm/data/surv_input_data.RData")
#' fileloc_out <- paste0("plots/post_pred_cfsep_exp_exp_", tx_name, ".png")
#' ##TODO: res <- readRDS("data/")  # where is this data?
#' plot_post_pred_KM(res, tx_name, orig_data, fileloc_out)
#'
plot_post_pred_KM <- function(res,
                              tx_name,
                              orig_data,
                              fileloc_out = NA,
                              casemix = TRUE) {

  real_data <- orig_data[orig_data$TRTA == tx_name, ]

  stan_extract <-
    rstan::extract(res) %>%
    lapply(drop)

  ##TODO: hack
  # n_post <- dim(stan_extract$t_os_tilde)[2] # stand-alone
  n_post <-
    max(dim(stan_extract$lambda_os_mean)[1],
        dim(stan_extract$lambda_os_tilde)[1], na.rm = TRUE)

  if (casemix) {
    # t_os <- stan_extract$t_os_tilde   # stand-alone
    # t_pfs <- stan_extract$t_pfs_tilde
    t_os <- stan_extract$t_os_tilde
    t_pfs <- stan_extract$t_pfs_tilde
  } else{
    t_os <- stan_extract$t_os_bar
    t_pfs <- stan_extract$t_pfs_bar
  }

  if (!is.na(fileloc_out)) {
    png(filename = fileloc_out)
    on.exit(dev.off())}

  par(mfrow = c(1,2))

  # os
  y_tilde <- t_os[1, ]

  plot(1,
       type = "n",
       col = "lightblue",
       xlim = c(0, 60),
       ylim = c(0,1),
       conf.int = FALSE,
       main = "OS",
       ylab = "Survival",
       xlab = "Month",
       bty = "n")

  for (i in seq_len(n_post)) {
    fit <- survfit(Surv(t_os[i, ], rep(1, length(y_tilde))) ~ 1)
    lines(fit, col = "lightblue", conf.int = FALSE)
  }

  fit <- survfit(Surv(real_data$os, real_data$os_event) ~ 1)
  lines(fit, lwd = 2.5, conf.int = FALSE)

  # pfs
  y_tilde <- t_pfs[1, ]

  plot(1,
       type = "n",
       col = "lightblue",
       xlim = c(0, 60),
       ylim = c(0,1),
       conf.int = FALSE,
       main = "PFS",
       xlab = "Month",
       ylab = "",
       bty = "n")

  for (i in seq_len(n_post)) {
    fit <- survfit(Surv(t_pfs[i, ], rep(1, length(y_tilde))) ~ 1)
    lines(fit, col = "lightblue", conf.int = FALSE)
  }

  # observed data
  fit <- survfit(Surv(real_data$pfs, real_data$pfs_event) ~ 1)
  lines(fit, lwd = 2.5, conf.int = FALSE)

  title(tx_name, line = -1, outer = TRUE)

  invisible(res)
}


# ##TODO:
# plot_postpred_ggplot <- function(res,
#                                  tx_name,
#                                  orig_data,
#                                  fileloc_out = NA,
#                                  casemix = TRUE) {
#
#   real_data <- orig_data[orig_data$TRTA == tx_name, ]
#   stan_extract <- rstan::extract(res)
#   # n_post <- dim(stan_extract$t_os_tilde)[2] # stand-alone
#   n_post <- dim(stan_extract$lambda_os_mean)[2]
#
#   if (casemix) {
#     # t_os <- stan_extract$t_os_tilde[1, , ]   # stand-alone
#     # t_pfs <- stan_extract$t_pfs_tilde[1, , ]
#     t_os <- stan_extract$t_os_tilde
#     t_pfs <- stan_extract$t_pfs_tilde
#   } else{
#     t_os <- stan_extract$t_os_bar[1, , ]
#     t_pfs <- stan_extract$t_pfs_bar[1, , ]
#   }
#
#   if (!is.na(fileloc_out)) {
#     png(filename = fileloc_out)
#     on.exit(dev.off())}
#
#   # os
#   y_tilde <- t_os[1, ]
#
#   for (i in seq_len(n_post)) {
#     fit <- survfit(Surv(t_os[i, ], rep(1, length(y_tilde))) ~ 1)
#   }
#
#   fit <- survfit(Surv(real_data$os, real_data$os_event) ~ 1)
#
#   # pfs
#   y_tilde <- t_pfs[1, ]
#
#   for (i in seq_len(n_post)) {
#     fit <- survfit(Surv(t_pfs[i, ], rep(1, length(y_tilde))) ~ 1)
#
#   }
#
#   # observed data
#   fit <- survfit(Surv(real_data$pfs, real_data$pfs_event) ~ 1)
#
#   ggplot(col = "lightblue",
#          xlim = c(0, 60),
#          ylim = c(0,1),
#          conf.int = FALSE,
#          main = "OS",
#          ylab = "Survival",
#          xlab = "Month",
#          bty = "n")
#   geom_lines(fit, col = "lightblue", conf.int = FALSE) +
#     geom_lines(fit, lwd = 2.5, conf.int = FALSE)
#   conf.int = FALSE,
#        main = "PFS")
#   geom_lines(fit, col = "lightblue", conf.int = FALSE)
#   geom_lines(fit, lwd = 2.5, conf.int = FALSE)
#   ggtitle(tx_name, line = -1, outer = TRUE)
# }


#' Plot Kaplan-Meier Posterior Predictions
#'
#' ggplot posterior predictions survival plots
#' for all treatments
#' Stan data from simultaneous treatment models (new version).
#'
#' @import reshape2 ggplot2 survival
#'
#' @examples
#'
#' tx_name <- "IPILIMUMAB"
#' orig_data <- load("~/R/rstanbmcm/data/surv_input_data.RData")
#' fileloc_out <- paste0("plots/post_pred_cfsep_exp_exp_", tx_name, ".png")
#' ##TODO: res <- readRDS("data/")  # where is this data?
#' plot_post_pred_KM(res, tx_name, orig_data, fileloc_out)
#'
plot_post_pred_Tx <- function(res,
                              orig_data,
                              fileloc_out = NA,
                              casemix = TRUE,
                              event_type = "os") {
  stan_extract <-
    rstan::extract(res) %>%
    lapply(drop)

  if (casemix) {
    t_os <- stan_extract$t_os_tilde
    t_pfs <- stan_extract$t_pfs_tilde
  } else{
    t_os <- stan_extract$t_os_bar
    t_pfs <- stan_extract$t_pfs_bar
  }

  event_times <- if (event_type == "os") t_os else t_pfs

  if (!is.na(fileloc_out)) {
    png(filename = fileloc_out)
    on.exit(dev.off())}

  nTx <- table(orig_data$TRTA, exclude = "")
  tx_names <- names(nTx)

  plot_dat <-
    event_times %>%
    `colnames<-`(rep(tx_names, times = nTx)) %>%
    melt() %>%
    as_tibble %>%
    rename(rep = "Var1", Tx = "Var2") %>%
    mutate(status = 1) %>%
    group_by(rep, Tx) %>%
    summarise(S = survfit(Surv(value, status) ~ 1)$surv,
              time = survfit(Surv(value, status) ~ 1)$time)

  ggplot(plot_dat, aes(time, S, group = rep)) +
    facet_grid(. ~ Tx) +
    geom_line() +
    xlim(0, 100) +
    theme_bw() +
    geom_kaplan_meier(orig_data,
                      col = "red",
                      event_type = event_type)
}

