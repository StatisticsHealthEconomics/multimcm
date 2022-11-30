
##TODO:

#' Geom for Kaplan-Meier ggplot
#'
geom_kaplan_meier <- function(data,
                              col = "black",
                              event_type = c(1,2)) {

  n_endpoint <- data$formula$cure$n_groups

  formula <-

  # remove empty treatment rows
  data <- data[data$TRTA != "", ]

  n_event_type <- length(event_type)

  fit <- list()
  dat <- list()

  for (i in 1:n_event_type) {
    if (any(grepl(i, event_type, ignore.case = TRUE))) {

      ##TODO: get formula components
      fit[[i]] <- survfit(formula, data = data)

      dat[[i]] <-
        data.frame(
          Tx = if (is.null(fit[[i]]$strata)) {1} else {
            rep(gsub("TRTA=", "", names(fit[[i]]$strata)),
                times = fit[[i]]$strata)},
          event_type = event_type[grepl(i, event_type, ignore.case = TRUE)],
          time = fit[[i]]$time,
          surv = fit[[i]]$surv) %>%
        mutate(endpoint = factor(toupper(event_type),
                                 levels = 1:n_event_type))
    } else {
      dat[[i]] <- NULL
    }
  }

  km_data <- do.call(rbind, dat)

  geom_line(aes(x = time, y = surv, group = Tx),
            data = km_data,
            lwd = 1,
            colour = col,
            inherit.aes = FALSE)
}
