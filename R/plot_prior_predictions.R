
#' @examples
#'
#' stan_out <- out
#' plot_prior_predictive(out, event_type = "os")
#' plot_prior_predictive(out, event_type = "pfs")
#'
plot_prior_predictive <- function(stan_out,
                                  event_type = NA) {

  stan_extract <- rstan::extract(stan_out)

  if (is.na(event_type)) {
    S_prior <- "S_prior"
    S_0 <- "pS_0"
  } else if (event_type == "os") {
    S_prior <- "S_os_prior"
    S_0 <- "pS_os"
  } else {
    S_prior <- "S_pfs_prior"
    S_0 <- "pS_pfs"
  }

  # rearrange to time as rows
  S_dat <-
    list(
      t(stan_extract[[S_prior]]) %>%
        as_tibble() %>%
        mutate(month = 1:n(),
               type = S_prior),
      t(stan_extract[[S_0]]) %>%
        as_tibble() %>%
        mutate(month = 1:n(),
               type = S_0),
      t(stan_extract$pS_bg) %>%
        as_tibble() %>%
        mutate(month = 1:n(),
               type = "S_bg"))

  # means and credible intervals
  S_stats <-
    S_dat %>%
    do.call(rbind, .) %>%
    melt(id.vars = c("month", "type")) %>%
    group_by(month, type) %>%
    summarise(mean = mean(value),
              lower = quantile(value, probs = 0.025),
              upper = quantile(value, probs = 0.975))

  ggplot(S_stats, aes(month, mean, group = type, colour = type)) +
    geom_line() +
    ylim(0, 1) +
    ylab("Survival") +
    geom_ribbon(aes(x = month, ymin = lower, ymax = upper, fill = type),
                linetype = 0,
                alpha = 0.2)
}

