
# cf_forest_plot(out)


library(ggplot2)

distns <- c(
  "exp",
  "weibull",
  "gompertz",
  "loglogistic",
  "lognormal")

bmcm_out <- list()

for (i in distns) {
  bmcm_out[[i]] <- readRDS(glue::glue("~/R/multimcm/data/bmcm_stan_{i}_{i}.Rds"))
}





