
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
source("R/cf_forest_plot.R")


data("surv_input_data")

trt <- "NIVOLUMAB+IPILIMUMAB"
# trt <- "IPILIMUMAB"
# trt <- "NIVOLUMAB"


################
# hierarchical #
################

# survival curves

p1 <-
  plot_S_grid(distns = c("exp", "weibull", "lognormal", "gompertz", "loglogistic"),
              folder = "data/independent/cf hier/bg_fixed_hr1",
              trt = trt,
              data = surv_input_data)

ggsave(p1, filename = glue::glue("plots/plot_S_grid_{trt}.png"),
       units = "in", width = 16, height = 13, dpi = 300)


# forest plots

p2 <- cf_forest_plot(trt = trt)

ggsave(p2, filename = glue::glue("plots/forest_plot_{trt}.png"),
       units = "in", width = 9, height = 10, dpi = 300)


# all treatments on single plot

## combine individual plots

g_ipi <- cf_forest_plot(trt = "IPILIMUMAB")
g_nivo <- cf_forest_plot(trt = "NIVOLUMAB")
g_nivo_ipi <- cf_forest_plot(trt = "NIVOLUMAB+IPILIMUMAB")

plot_format <-
  theme(text = element_text(size = 20)) +
  xlim(0, 0.6)

g_ipi <- g_ipi + plot_format + ggtitle("(i)")
g_nivo <- g_nivo + plot_format + ggtitle("(ii)")
g_nivo_ipi <- g_nivo_ipi + plot_format + ggtitle("(iii)")

p3 <-
  do.call("grid_arrange_shared_legend",
          c(list(g_ipi, g_nivo, g_nivo_ipi),
            nrow = 1, ncol = 3, position = "top"))

ggsave(p3, filename = glue::glue("plots/forest_plot_all_tx.png"),
       units = "in", width = 9, height = 10, dpi = 300)

## all treatment Stan output

p4 <- cf_forest_plotTx()



############
# separate #
############


