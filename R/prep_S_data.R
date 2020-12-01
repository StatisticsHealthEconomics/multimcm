
#' prep_S_data
#' 
#' @examples 
#' 
#' stan_out <- rstan::extract(out)
#' #' plot_dat <- prep_S_data(stan_out, event_type = "os")
#' 
#' ggplot(plot_dat, aes(month, mean, group = type, colour = type)) +
#' geom_line() +
#' ylab("Survival") +
#' geom_ribbon(aes(x = month, ymin = lower, ymax = upper, fill = type),
#'             linetype = 0,
#'             alpha = 0.2)
#'             
prep_S_data <- function(stan_extract,
                        event_type = NA) {
  
  if (is.na(event_type)) {
    S_pred <- "S_pred"
    S_0 <- "S_0"
  } else if (event_type == "os") {
    S_pred <- "S_os_pred"
    S_0 <- "S_os"
  } else {
    S_pred <- "S_pfs_pred"
    S_0 <- "S_pfs"
  }
  
  # rearrange to time as rows
  S_dat <-
    list(
      t(stan_extract[[S_pred]]) %>%
        as_tibble() %>%
        mutate(month = 1:n(),
               type = S_pred),
      t(stan_extract[[S_0]]) %>%
        as_tibble() %>%
        mutate(month = 1:n(),
               type = S_0),
      t(stan_extract$S_bg) %>%
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
  
  S_stats
}


