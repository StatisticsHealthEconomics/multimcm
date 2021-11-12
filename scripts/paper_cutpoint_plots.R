
# paper output plots
# for different censoring
# cut-points


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


data("surv_data_cut")


################
# hierarchical #
################

###################
# survival curves

ghier30 <-
  plot_S_gridTx(distns = list(c("exp", "exp"),
                              c("lognormal", "lognormal")),
                folder = here::here("data", "independent", "cf hier", "bg_fixed_hr1"),
                data = surv_data_cut$`30`,
                save_name = "_cpt_30m",
                n_dim = c(1,2))

ggsave(ghier30, filename = glue::glue("plots/plot_S_grid_cf_hier_cpt_30m.png"),
       units = "in", width = 16, height = 13, dpi = 300)


ghier12 <-
  plot_S_gridTx(distns = list(c("exp", "exp"),
                              c("lognormal", "lognormal")),
                folder = "data/independent/cf hier/bg_fixed_hr1",
                data = surv_data_cut$`12`,
                save_name = "_cpt_12m",
                n_dim = c(1,2))

ggsave(ghier12, filename = glue::glue("plots/plot_S_grid_cf_hier_cpt_12m.png"),
       units = "in", width = 16, height = 13, dpi = 300)


#################
# forest plots

ghier5 <- cf_forest_cutpoint()
ghier5

ggsave(ghier5, filename = glue::glue("plots/forest_plot_joint_cf_hier.png"),
       units = "in", width = 10, height = 8, dpi = 300)


############
# separate #
############

###################
# survival curves

gsep30 <-
  plot_S_gridTx(distns = list(c("exp", "exp"),
                              c("lognormal", "lognormal")),
                folder = "data/independent/cf separate/bg_fixed_hr1",
                data = surv_data_cut$`30`,
                save_name = "_cpt_30m",
                n_dim = c(1,2))

ggsave(gsep30, filename = glue::glue("plots/plot_S_grid_cf_sep_cpt_30m.png"),
       units = "in", width = 16, height = 13, dpi = 300)


gsep12 <-
  plot_S_gridTx(distns = list(c("exp", "exp"),
                              c("lognormal", "lognormal")),
                folder = "data/independent/cf separate/bg_fixed_hr1",
                data = surv_data_cut$`12`,
                save_name = "_cpt_12m",
                n_dim = c(1,2))

ggsave(gsep12, filename = glue::glue("plots/plot_S_grid_cf_sep_cpt_12m.png"),
       units = "in", width = 16, height = 13, dpi = 300)


################
# forest plots

ghier5 <- cf_forest_cutpoint()


