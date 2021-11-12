
#' geom_kaplan_meier
#'
#' by treatment
#'
geom_kaplan_meier <- function(data,
                              col = "black",
                              event_type = c("os", "pfs")) {

  # remove empty treatment rows
  data <- data[data$TRTA != "", ]

  if (any(grepl("os", event_type))) {
    fit_os <- survfit(Surv(os, os_event) ~ TRTA, data = data)

    os_dat <-
      data.frame(
        Tx = if (is.null(fit_os$strata)) {1} else {
          rep(gsub("TRTA=", "", names(fit_os$strata)),
              times = fit_os$strata)},
        event_type = event_type[grepl("os", event_type)],
        time = fit_os$time,
        surv = fit_os$surv)
  } else {
    os_dat <- NULL
  }

  if (any(grepl("pfs", event_type))) {
    fit_pfs <- survfit(Surv(pfs, pfs_event) ~ TRTA, data = data)

    pfs_dat <-
      data.frame(
        Tx =  if (is.null(fit_pfs$strata)) {1} else {
          rep(gsub("TRTA=", "", names(fit_pfs$strata)),
              times = fit_pfs$strata)},
        event_type = event_type[grepl("pfs", event_type)],
        time = fit_pfs$time,
        surv = fit_pfs$surv)
  } else {
    pfs_dat <- NULL
  }

  km_data <- rbind(os_dat, pfs_dat)

  geom_line(aes(x = time, y = surv, group = Tx),
            data = km_data,
            lwd = 1,
            colour = col,
            inherit.aes = FALSE)
}

