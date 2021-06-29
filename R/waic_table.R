
##TODO: remove duplication..
#
waic_table <- function(log_lik) {

  tab <-
    purrr::map(log_lik,
               ~round(waic(.x)[["estimates"]], 2)) %>%
    do.call(rbind, .) %>%
    data.frame(Statistic = c("elpd_waic","p_waic","waic"),
               scenarios = rep(names(stan_list), each = 3)) %>%
    tidyr::separate(scenarios,
                    into = c("OS distn", "PFS distn"), sep = "_") %>%
    relocate(where(is.character)) %>%
    arrange(Statistic)

  tab <-
    data.table::dcast(data = setDT(tab),
                      `OS distn` + `PFS distn` ~ Statistic,
                      value.var = c("Estimate", "SE")) %>%
    relocate("SE_elpd_waic", .after = "Estimate_elpd_waic") %>%
    relocate("SE_p_waic", .after = "Estimate_p_waic") %>%
    relocate("SE_waic", .after = "Estimate_waic")

  tab
}


#
loo_table <- function(log_lik) {

  tab <-
    purrr::map(log_lik,
               ~round(loo(.x)[["estimates"]], 2)) %>%
    do.call(rbind, .) %>%
    data.frame(Statistic = factor(c("elpd_loo", "p_loo", "looic")),
               scenarios = rep(names(stan_list), each = 3)) %>%
    tidyr::separate(scenarios,
                    into = c("OS distn", "PFS distn"), sep = "_") %>%
    relocate(where(is.character)) %>%
    mutate(Statistic = factor(Statistic, levels = c("elpd_loo","p_loo","looic"))) %>%
    arrange(Statistic)

  tab <-
    data.table::dcast(data = setDT(tab),
                    `OS distn` + `PFS distn` ~ Statistic,
                    value.var = c("Estimate", "SE")) %>%
    relocate("SE_elpd_loo", .after = "Estimate_elpd_loo") %>%
    relocate("SE_p_loo", .after = "Estimate_p_loo") %>%
    relocate("SE_looic", .after = "Estimate_looic")

  tab
}

