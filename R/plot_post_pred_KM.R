
#' Plot posterior predictive Kaplan-Meier
#'
#' Base R used.
#' stand-alone generated values as input.
#'
#' @param res Stan output
#' @param tx_name "IPILIMUMAB", "NIVOLUMAB", "NIVOLUMAB+IPILIMUMAB"
#' @param orig_data Original study data
#' @param fileloc_out File address for output plot with extension
#' @param casemix Use case-mix rates or overall mean
#'
#' @return
#'
#' @import survival
#' @export
#'
#' @examples
#'
#' tx_name <- "IPILIMUMAB"
#' orig_data <- load("~/R/rstanbmcm/data/surv_input_data.RData")
#' fileloc_out <- paste0("plots/post_pred_cfsep_exp_exp_", tx_name, ".png")
#' plot_post_pred_KM(tx_name, orig_data, fileloc_out)
#'
plot_post_pred_KM <- function(res,
                              tx_name,
                              orig_data,
                              fileloc_out = NA,
                              casemix = TRUE) {
  real_data <- orig_data[orig_data$TRTA == tx_name, ]
  yy <- rstan::extract(res)
  # n_post <- dim(yy$t_os_tilde)[2] # stand-alone
  n_post <- dim(yy$t_os_tilde)[1]

  if (casemix) {
    # t_os <- yy$t_os_tilde[1, , ]   # stand-alone
    # t_pfs <- yy$t_pfs_tilde[1, , ]
    t_os <- yy$t_os_tilde
    t_pfs <- yy$t_pfs_tilde
  } else{
    t_os <- yy$t_os_bar[1, , ]
    t_pfs <- yy$t_pfs_bar[1, , ]
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

