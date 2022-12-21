
#' Geom for Kaplan-Meier ggplot
#'
geom_kaplan_meier <- function(out_dat,
                              col = "black",
                              add_marks = TRUE) {

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
        d = as.numeric(fit[[i]]$n.event>0),
        surv = fit[[i]]$surv)
    # } else {
    #   dat[[i]] <- NULL
    # }
  }

  km_data <- do.call(rbind, dat)

  # include time 0 survival
  origin_vals <-
    expand.grid(unique(km_data$Tx),
                unique(km_data$endpoint)) |>
    rename(Tx = Var1,
           endpoint = Var2) |>
    cbind(time = 0,
          surv = 1,
          d = 1)

  km_data <- km_data |>
    rbind(origin_vals) |>
    arrange(endpoint, Tx, time)

  if (add_marks) {
    cens_dat <- km_data[km_data$d == 0, ]
    geom_marks <- geom_text(aes(x = time, y = surv, group = Tx),
                            data = cens_dat, label = "+", inherit.aes = FALSE)
  } else {geom_marks <- NULL}

  list(geom_step(aes(x = time, y = surv, group = Tx),
                 data = km_data,
                 lwd = 1,
                 colour = col,
                 inherit.aes = FALSE),
       geom_marks)
}
