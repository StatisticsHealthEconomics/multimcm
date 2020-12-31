
#' plot_post_pred_KM
#' 
#' @param res Stan output
#' @param tx_name "IPILIMUMAB", "NIVOLUMAB", "NIVOLUMAB+IPILIMUMAB"
#' @param orig_data Original study data
#' @param fileloc_out File address for output plot with extension
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
                              fileloc_out) {
  
  n_post <- 90
  real_data <- orig_data[orig_data$TRTA == tx_name, ]
  yy <- rstan::extract(res)
  
  png(filename = fileloc_out)
  par(mfrow = c(1,2))
  
  # os
  y_tilde <- yy$t_os_tilde[1, 1, ]
  fit <- survfit(Surv(y_tilde, rep(1, length(y_tilde))) ~ 1)
  plot(fit,
       col = "lightblue",
       xlim = c(0, 60),
       conf.int = FALSE,
       main = "OS",
       ylab = "Survival",
       xlab = "Month",
       bty = "n")
  for (i in 2:n_post) {
    fit <- survfit(Surv(yy$t_os_tilde[1, i, ], rep(1, length(y_tilde))) ~ 1)
    lines(fit, col = "lightblue", conf.int = FALSE)
  }
  
  fit <- survfit(Surv(real_data$os, real_data$os_event) ~ 1)
  lines(fit, lwd = 2.5, conf.int = FALSE)
  
  # pfs
  y_tilde <- yy$t_pfs_tilde[1, 1, ]
  fit <- survfit(Surv(y_tilde, rep(1, length(y_tilde))) ~ 1)
  plot(fit,
       col = "lightblue",
       xlim = c(0, 60),
       conf.int = FALSE,
       main = "PFS",
       xlab = "Month",
       bty = "n")
  
  for (i in 2:n_post) {
    fit <- survfit(Surv(yy$t_pfs_tilde[1, i, ], rep(1, length(y_tilde))) ~ 1)
    lines(fit, col = "lightblue", conf.int = FALSE)
  }
  
  fit <- survfit(Surv(real_data$pfs, real_data$pfs_event) ~ 1)
  lines(fit, lwd = 2.5, conf.int = FALSE)
  
  title(tx_name, line = -1, outer = TRUE)
  dev.off()
  
  return()
}

