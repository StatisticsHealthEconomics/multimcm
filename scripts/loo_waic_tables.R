
# model checking
# cross-validation
#
# at the moment print kable() and then paste
# into Check_mate_analysis.Rmd
# TODO: read in table directly to RMarkdown


library(rstanarm)
library(bayesplot)
library(loo)
library(purrr)
library(dplyr)
library(tibble)
library(reshape2)
library(data.table)


source("R/waic_table.R")

cf_models <- c("cf hier", "cf separate")

## select model

cf_model <- "cf hier"
# cf_model <- "cf separate"

bg_model <- "bg_fixed_hr1"
# bg_model <- "bg_fixed_hr1.63"

folder <- glue::glue("data/independent/{cf_model}/{bg_model}")


##########################
# single treatments

scenarios <-
  dir(folder) %>%
  gsub("stan_", "", .) %>%
  gsub(".Rds", "", .)

res_ls <-
  dir(data_dir, full.names = TRUE) %>%
  purrr::map(readRDS) %>%
  setNames(scenarios)

# purrr::map(res_ls, loo, cores = 2)
# plot(loo_ipi)


## waic table

log_lik <-
  purrr::map(res_ls, extract_log_lik)

tab <-
  purrr::map(log_lik,
             ~round(waic(.x)[["estimates"]], 2)) %>%
  do.call(rbind, .) %>%
  data.frame(Statistic = c("elpd_waic","p_waic","waic"),
             scenarios = rep(scenarios, each = 3)) %>%
  tidyr::separate(scenarios,
                  into = c("OS distn", "PFS distn", "Treatment"), sep = "_") %>%
  relocate(where(is.character)) %>%
  # rownames_to_column(var = "Statistic") %>%
  arrange(Statistic)

knitr::kable(tab, row.names = FALSE)


##################################
## all treatments Stan output

# load all files
keep_files <- !grepl("[IPI | NIVO]", list.files(folder))
filenames <- list.files(folder, full.names = TRUE)
filenames <- filenames[keep_files]

stan_list <- map(filenames, readRDS)

names(stan_list) <-
  gsub(".Rds", "", list.files(folder, full.names = FALSE)[keep_files]) %>%
  gsub("stan_", "", .)

log_lik <-
  map(stan_list, extract_log_lik)

waic_tab <- waic_table(log_lik)

knitr::kable(waic_tab,
             row.names = FALSE,
             format = "latex",
             escape = FALSE)

loo_tab <- loo_table(log_lik)

knitr::kable(loo_tab,
             row.names = FALSE,
             format = "latex",
             escape = FALSE)


#########################
# sep and hier together

# load all files
log_lik <- list()

for (i in cf_models) {

  folder <- glue::glue("data/independent/{i}/{bg_model}")
  keep_files <- !grepl("[IPI | NIVO]", list.files(folder))
  filenames <- list.files(folder, full.names = TRUE)
  filenames <- filenames[keep_files]

  stan_list <- map(filenames, readRDS)

  names(stan_list) <-
    gsub(".Rds", "", list.files(folder, full.names = FALSE)[keep_files]) %>%
    gsub("stan_", "", .)

  log_lik[[i]] <-
    map(stan_list, extract_log_lik)
}

tab <- map(log_lik, waic_table)

plyr::join_all(tab, by = c("OS distn", "PFS distn")) %>%
knitr::kable(row.names = FALSE,
             format = "latex",
             escape = FALSE)



# -------------------------------------------------------------------------

# r_eff <- relative_eff(exp(log_lik$exp_exp), chain_id = 1:360, cores = 2)
# loo_1 <- loo(log_lik$exp_exp)#, r_eff = r_eff, cores = 2)


## variance partition coefficients

# vpc <- function(out) {
#   x <- extract(out)
#   c(vpc_pfs = round(sd(x$lp_cf_global)^2/(sd(x$lp_cf_global)^2 + sd(x$lp_cf_pfs)^2), 3),
#     vpc_os = round(sd(x$lp_cf_global)^2/(sd(x$lp_cf_global)^2 + sd(x$lp_cf_os)^2), 3))
# }
#
# knitr::kable(
#   cbind(scenarios,
#         purrr::map_df(res_hier_fixed, vpc)))

