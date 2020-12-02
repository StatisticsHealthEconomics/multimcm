
#' curefraction_boxplots
#'
#' do for OS, PFS
#'
#' @param stan_out stan model output
#'
curefraction_boxplots <- function(stan_out) {

  # data prep
  dat <-
    map_dfc(stan_out,
           function(x) rstan::extract(x)$curefrac) %>%
    as.matrix() %>%
    melt() %>%
    rename(Treatment = Var2,
           "Cure fraction" = value)

    # group_by(Var2) %>%
    # summarise(mean = mean(value),
    #           lower = quantile(value, probs = 0.025),
    #           upper = quantile(value, probs = 0.975))

  ggplot(dat, aes(x=Treatment, y=`Cure fraction`, color=Treatment)) +
    geom_boxplot() +
    ylim(0,1)
}

