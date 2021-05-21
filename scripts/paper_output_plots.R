
# paper output plots
#


# marginal model ----
# cure fraction

# xx <- extract(out)
# hist(xx$curefrac,
#      breaks = 30,
#      freq = FALSE,
#      xlim = c(0,1),
#      xlab = "probability",
#      main = "cure fraction")
# lines(density(xx$pmean_cf), col = "red")
# lines(density(xx$curefrac), col = "blue")

library(dplyr)
library(survival)
library(reshape2)
library(purrr)
library(ggplot2)
library(gridExtra)
library(grid)

source("R/plot_S_grid.R")
source("R/grid_arrange_shared_legend.R")
source("R/plot_S_joint.R")
source("R/prep_S_data.R")
source("R/stan_forest_plot_Tx")


data("surv_input_data")

trt <- "NIVOLUMAB+IPILIMUMAB"
# trt <- "IPILIMUMAB"
# trt <- "NIVOLUMAB"

p <-
  plot_S_grid(distns = c("exp", "weibull", "lognormal", "gompertz", "loglogistic"),
              folder = "data/independent/cf hier/bg_fixed_hr1",
              trt = trt,
              data = surv_input_data)

ggsave(p, filename = glue::glue("plots/plot_S_grid_{trt}.png"),
       units = "in", width = 16, height = 13, dpi = 300)


p <-
  stan_forest_plot_Tx(trt = trt)

ggsave(p, filename = glue::glue("plots/forest_plot_{trt}.png"),
       units = "in", width = 9, height = 10, dpi = 300)
