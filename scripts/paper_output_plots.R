
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
source("R/plot_S_gridTx.R")
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

###################
# survival curves

stan_exp_exp <-
  readRDS("~/R/rstanbmcm/data/independent/cf hier/bg_fixed_hr1/stan_exp_exp.Rds")

ghier1 <- plot_S_jointTx(stan_out = stan_exp_exp,
                         annot_cf = FALSE,
                         data = surv_input_data)

ghier1

ghier2 <-
  plot_S_gridTx(distns = c("exp", "weibull", "lognormal", "gompertz", "loglogistic"),
                folder = "data/independent/cf hier/bg_fixed_hr1",
                data = surv_input_data)

ggsave(ghier2, filename = glue::glue("plots/plot_S_grid_cf_hier.png"),
       units = "in", width = 16, height = 13, dpi = 300)


#################
# forest plots

ghier3 <- cf_forest_plot(trt = trt)

ggsave(ghier3, filename = glue::glue("plots/forest_plot_{trt}.png"),
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

ghier4 <-
  do.call("grid_arrange_shared_legend",
          c(list(g_ipi, g_nivo, g_nivo_ipi),
            nrow = 1, ncol = 3, position = "top"))

ggsave(ghier4, filename = glue::glue("plots/forest_plot_all_tx.png"),
       units = "in", width = 9, height = 10, dpi = 300)

## all treatment Stan output

ghier5 <- cf_forest_plotTx()
ghier5

ggsave(ghier5, filename = glue::glue("plots/forest_plot_joint_cf_hier.png"),
       units = "in", width = 10, height = 8, dpi = 300)


############
# separate #
############

###################
# survival curves

stan_exp_exp <-
  readRDS("~/R/rstanbmcm/data/independent/cf separate/bg_fixed_hr1/stan_exp_exp.Rds")

gsep1 <- plot_S_jointTx(stan_out = stan_exp_exp,
                        annot_cf = FALSE,
                        data = surv_input_data)
gsep1

gsep2 <-
  plot_S_gridTx(distns = c("exp", "weibull", "lognormal"),#, "gompertz", "loglogistic"),
                folder = "data/independent/cf separate/bg_fixed_hr1",
                data = surv_input_data)

ggsave(gsep2, filename = glue::glue("plots/plot_S_grid_cf_separate.png"),
       units = "in", width = 16, height = 13, dpi = 300)


################
# forest plots

gsep3 <- cf_forest_plotTx(folder = "data/independent/cf separate/bg_fixed_hr1")

ggsave(gsep3, filename = glue::glue("plots/forest_plot_joint_all_tx_separate.png"),
       units = "in", width = 10, height = 8, dpi = 300)

