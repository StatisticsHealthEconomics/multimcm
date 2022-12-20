
#' Geom for Kaplan-Meier ggplot
#'
geom_kaplan_meier <- function(out_dat,
                              col = "black") {

  n_groups <- out_dat$formula$cure$n_groups
  group_var <- out_dat$formula$cure$group_var
  fe_var <- out_dat$formula$cure$fe_vars

  formula <-
    update.formula(out_dat$formula$latent$lhs_form,
                   out_dat$formula$cure$fe_form)

  fit <- list()
  dat <- list()

  for (i in 1:n_groups) {
    # if (any(grepl(i, event_type, ignore.case = TRUE))) {

    group_dat <- out_dat$input_data[out_dat$input_data[[group_var]] == i, ]
    fit[[i]] <- survfit(formula, data = group_dat)

    # convert to ggplot long format
    dat[[i]] <-
      data.frame(
        Tx =
          if (is.null(fit[[i]]$strata)) {1
          } else {
            rep(gsub(paste0(fe_var,"="), "", names(fit[[i]]$strata)),
                times = fit[[i]]$strata)},
        endpoint = i,   #event_type[grepl(i, event_type, ignore.case = TRUE)],
        time = fit[[i]]$time,
        surv = fit[[i]]$surv)
    # } else {
    #   dat[[i]] <- NULL
    # }
  }

  km_data <- do.call(rbind, dat)
    browser()

  geom_line(aes(x = time, y = surv, group = Tx),
            data = km_data,
            lwd = 1,
            colour = col,
            inherit.aes = FALSE)
}
