
# 60 months prediction table
# for different data cut points
# for paper


###############
# hierarchical

folder <- here::here("data/dbl_cut/hier/")

# read in data
filenames <- list.files(folder, full.names = TRUE)
keep_files <- grepl(pattern = "bmcm_stan_[a-z]+_[a-z]+_\\d+.Rds", filenames)
filenames <- filenames[keep_files]
stan_list <- map(filenames, readRDS)
names(stan_list) <-
  gsub(".Rds", "", list.files(folder, full.names = FALSE)[keep_files])

par_names <- c("S_1_pred[60,1]", "S_1_pred[60,2]", "S_1_pred[60,3]",
               "S_2_pred[60,1]", "S_2_pred[60,2]", "S_2_pred[60,3]")

dat_hier <- map(stan_list,
                function(x) as_tibble(rstan::summary(x$output, par = par_names)$summary))

hier_tab <-
  dplyr::bind_rows(dat_hier, .id = "model") |>
  tidyr::separate("model", c("a", "b","c", "distn", "ctpt"), sep = "_") |>
  select(-(a:c)) |>
  mutate(model = "hier",
         endpoint = rep(c("OS", "PFS"), n()/2),
         drug = rep(1:3, n()/3),
         ctpt = factor(ctpt, levels = c("12", "30", "100"))) |>
  select(model, endpoint, drug, ctpt, everything())

###########
# separate

folder <- here::here("data/dbl_cut/separate/")
filenames <- list.files(folder, full.names = TRUE)
keep_files <- grepl(pattern = "bmcm_stan_[a-z]+_[a-z]+_\\d+.Rds", filenames)
filenames <- filenames[keep_files]
stan_list <- map(filenames, readRDS)
names(stan_list) <-
  gsub(".Rds", "", list.files(folder, full.names = FALSE)[keep_files])

par_names <- c("S_1_pred[60,1]", "S_1_pred[60,2]", "S_1_pred[60,3]",
               "S_2_pred[60,1]", "S_2_pred[60,2]", "S_2_pred[60,3]")

dat_sep <- map(stan_list,
           function(x) as_tibble(rstan::summary(x$output, par = par_names)$summary))

sep_tab <-
  dplyr::bind_rows(dat_sep, .id = "model") |>
  tidyr::separate("model", c("a", "b","c", "distn", "ctpt"), sep = "_") |>
  select(-(a:c)) |>
  mutate(model = "separate",
         endpoint = rep(c("OS", "PFS"), n()/2),
         drug = rep(1:3, n()/3),
         ctpt = factor(ctpt, levels = c("12", "30", "100"))) |>
  select(model, endpoint, ctpt, drug, everything())

tab <-
  rbind(sep_tab, hier_tab) |>
  mutate(across(mean:Rhat, \(x) round(x, 2)),
         mean_ci = glue::glue("{mean} [{`2.5%`}, {`97.5%`}]")) |>
  reshape2::dcast(endpoint+model+distn+ctpt~drug, value.var = "mean_ci") |>
  rename("Ipilimumab" = "1", "Nivolumab" = "2", "Combined" = "3")


knitr::kable(tab, format = "latex")
