
# ratio of 95% CrI for hierarchical and
# separate model cure fractions


library(rstanarm)
library(bayesplot)
library(loo)
library(purrr)
library(dplyr)
library(tibble)


## select model

bg_model <- "bg_fixed_hr1"
# bg_model <- "bg_fixed_hr1.63"

dir_sep <- glue::glue("data/independent/cf separate/{bg_model}")
dir_hier <- glue::glue("data/independent/cf hier/{bg_model}")


## load all Stan output

keep_sep <- !grepl("[IPI | NIVO]", list.files(dir_sep))
filenames_sep <- list.files(dir_sep, full.names = TRUE)
filenames_sep <- filenames_sep[keep_sep]

keep_hier <- !grepl("[IPI | NIVO]", list.files(dir_hier))
filenames_hier <- list.files(dir_hier, full.names = TRUE)
filenames_hier <- filenames_hier[keep_hier]

stan_sep <- map(filenames_sep, readRDS)
stan_hier <- map(filenames_hier, readRDS)

names(stan_sep) <-
  gsub(".Rds", "", list.files(dir_sep, full.names = FALSE)[keep_sep]) %>%
  gsub("stan_", "", .)

names(stan_hier) <-
  gsub(".Rds", "", list.files(dir_hier, full.names = FALSE)[keep_hier]) %>%
  gsub("stan_", "", .)


## 95% credible interval for cure fractions

CrI95 <- function(dat) {
  extract(dat) %>%
    .[c("cf_os", "cf_pfs")] %>%
    do.call(cbind, .) %>%
    `colnames<-`(c("cf_os_ipi", "cf_os_nivo", "cf_os_comb",
                   "cf_pfs_ipi", "cf_pfs_nivo", "cf_pfs_comb")) %>%
    reshape2::melt() %>%
    group_by(Var2) %>%
    summarise(value = quantile(value, c(0.025, 0.975)),
              q = c(2.5, 97.5)) %>%
    summarise(CrI95 = diff(value))
}

hier_CrI95 <- map(stan_hier, CrI95)
sep_CrI95 <- map(stan_sep, CrI95)

map2(hier_CrI95, sep_CrI95,
     ~ merge(.x, .y,
             by = "Var2",
             suffixes = c(".h", ".s")) %>%
       mutate(ratio = CrI95.h/CrI95.s))


# rstanarm::posterior_interval(
#   stan_sep$exp_exp, pars = c("cf_os", "cf_pfs"), prob = 0.025)



