
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
source("R/plot_S_jointTx.R")
source("R/prep_S_data.R")
source("R/prep_S_dataTx.R")
source("R/cf_forest_plot.R")
source("R/cf_forest_plotTx.R")


data("surv_input_data")

trt <- "NIVOLUMAB+IPILIMUMAB"
# trt <- "IPILIMUMAB"
# trt <- "NIVOLUMAB"


################
# hierarchical #
################

# survival curves

stan_exp_exp <-
  readRDS("~/R/rstanbmcm/data/independent/cf hier/bg_fixed_hr1/stan_exp_exp.Rds")

gg <- plot_S_jointTx(stan_out = stan_exp_exp,
                     annot_cf = FALSE,
                     data = surv_input_data)

gg

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
  theme(text = element_text(size = 20))

g_ipi <- g_ipi + plot_format + ggtitle("(i)") + xlim(0, 0.6)
g_nivo <- g_nivo + plot_format + ggtitle("(ii)") + xlim(0, 0.6)
g_nivo_ipi <- g_nivo_ipi + plot_format + ggtitle("(iii)") + xlim(0, 0.6)

p3 <-
  do.call("grid_arrange_shared_legend",
          c(list(g_ipi, g_nivo, g_nivo_ipi),
            nrow = 1, ncol = 3, position = "top"))

ggsave(p3, filename = glue::glue("plots/forest_plot_all_tx.png"),
       units = "in", width = 9, height = 10, dpi = 300)

## all treatment Stan output

p4 <- cf_forest_plotTx()

ggsave(p4, filename = glue::glue("plots/forest_plot_joint_all_tx.png"),
       units = "in", width = 10, height = 8, dpi = 300)



############
# separate #
############

# survival curves

stan_exp_exp <-
  readRDS("~/R/rstanbmcm/data/independent/cf separate/bg_fixed_hr1/stan_exp_exp.Rds")

gg <- plot_S_jointTx(stan_out = stan_exp_exp,
                     annot_cf = FALSE,
                     data = surv_input_data)
gg

## forest plots

p5 <- cf_forest_plotTx(folder = "~/R/rstanbmcm/data/independent/cf separate/bg_fixed_hr1")

ggsave(p5, filename = glue::glue("plots/forest_plot_joint_all_tx_separate.png"),
       units = "in", width = 10, height = 8, dpi = 300)
