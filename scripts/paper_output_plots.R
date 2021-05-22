
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
source("R/stan_forest_plot_Tx.R")


data("surv_input_data")

trt <- "NIVOLUMAB+IPILIMUMAB"
# trt <- "IPILIMUMAB"
# trt <- "NIVOLUMAB"

p1 <-
  plot_S_grid(distns = c("exp", "weibull", "lognormal", "gompertz", "loglogistic"),
              folder = "data/independent/cf hier/bg_fixed_hr1",
              trt = trt,
              data = surv_input_data)

ggsave(p1, filename = glue::glue("plots/plot_S_grid_{trt}.png"),
       units = "in", width = 16, height = 13, dpi = 300)


p2 <- stan_forest_plot_Tx(trt = trt)

ggsave(p2, filename = glue::glue("plots/forest_plot_{trt}.png"),
       units = "in", width = 9, height = 10, dpi = 300)

g_ipi <- stan_forest_plot_Tx(trt = "IPILIMUMAB")
g_nivo <- stan_forest_plot_Tx(trt = "NIVOLUMAB")
g_nivo_ipi <- stan_forest_plot_Tx(trt = "NIVOLUMAB+IPILIMUMAB")

g_ipi <- g_ipi + theme(text = element_text(size=20)) + xlim(0, 0.6) + ggtitle("(i)")
g_nivo <- g_nivo + theme(text = element_text(size=20)) + xlim(0, 0.6) + ggtitle("(ii)")
g_nivo_ipi <- g_nivo_ipi + theme(text = element_text(size=20)) + xlim(0, 0.6) + ggtitle("(iii)")

do.call("grid_arrange_shared_legend",
        c(list(g_ipi, g_nivo, g_nivo_ipi), nrow = 1, ncol = 3, position = "top"))
