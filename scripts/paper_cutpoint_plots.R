
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
source("R/cf_forest_cutpoint.R")
source("R/prep_S_jointTx_data.R")
source("R/geom_kaplan_meier.R")

data("surv_data_cut", package = "rstanbmcm")


################
# hierarchical #
################

fig_height <- 10
fig_width <- 10

###################
# survival curves

ghier30 <-
  plot_S_gridTx(distns = list(c("exp", "exp"),
                              c("lognormal", "lognormal")),
                folder = here::here("data", "independent", "cf hier", "bg_fixed_hr1"),
                data = surv_data_cut$`30`,
                save_name = "_cpt_30m",
                n_dim = c(2,1))

ggsave(ghier30, filename = glue::glue("plots/plot_S_grid_cf_hier_cpt_30m.png"),
       units = "in", width = fig_width, height = fig_height, dpi = 300)


ghier12 <-
  plot_S_gridTx(distns = list(c("exp", "exp"),
                              c("lognormal", "lognormal")),
                folder = here::here("data", "independent", "cf hier", "bg_fixed_hr1"),
                data = surv_data_cut$`12`,
                save_name = "_cpt_12m",
                n_dim = c(2,1))

ggsave(ghier12, filename = glue::glue("plots/plot_S_grid_cf_hier_cpt_12m.png"),
       units = "in", width = fig_width, height = fig_height, dpi = 300)


#################
# forest plots

forest_hier <-
  cf_forest_cutpoint(folder = "data/independent/cf hier/bg_fixed_hr1",
                     is_hier = TRUE)
forest_hier

ggsave(forest_hier, filename = glue::glue("plots/forest_plot_cf_hier_cpt.png"),
       units = "in", width = 16, height = 14, dpi = 300)


############
# separate #
############

###################
# survival curves

gsep30 <-
  plot_S_gridTx(distns = list(c("exp", "exp"),
                              c("lognormal", "lognormal")),
                folder = here::here("data", "independent", "cf separate", "bg_fixed_hr1"),
                data = surv_data_cut$`30`,
                save_name = "_cpt_30m",
                n_dim = c(2,1))

ggsave(gsep30, filename = glue::glue("plots/plot_S_grid_cf_sep_cpt_30m.png"),
       units = "in", width = fig_width, height = fig_height, dpi = 300)


gsep12 <-
  plot_S_gridTx(distns = list(c("exp", "exp"),
                              c("lognormal", "lognormal")),
                folder = here::here("data", "independent", "cf separate", "bg_fixed_hr1"),
                data = surv_data_cut$`12`,
                save_name = "_cpt_12m",
                n_dim = c(2,1))

ggsave(gsep12, filename = glue::glue("plots/plot_S_grid_cf_sep_cpt_12m.png"),
       units = "in", width = fig_width, height = fig_height, dpi = 300)


################
# forest plots

forest_sep <-
  cf_forest_cutpoint(folder = "data/independent/cf separate/bg_fixed_hr1",
                     is_hier = FALSE)
forest_sep

ggsave(forest_sep, filename = glue::glue("plots/forest_plot_cf_sep_cpt.png"),
       units = "in", width = 16, height = 14, dpi = 300)

