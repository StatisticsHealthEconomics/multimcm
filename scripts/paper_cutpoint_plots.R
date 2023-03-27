
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


fig_height <- 10
fig_width <- 10

################
# hierarchical #
################

folder <- here::here("data/dbl_cut/hier/")

# read in data
filenames <- list.files(folder, full.names = TRUE)
keep_files <- grepl(pattern = "bmcm_stan_[a-z]+_[a-z]+_\\d+.Rds", filenames)
filenames <- filenames[keep_files]
stan_list <- map(filenames, readRDS)
names(stan_list) <-
  gsub(".Rds", "", list.files(folder, full.names = FALSE)[keep_files])

###################
# survival curves

gg_12 <- list()

gg_12[[1]] <- plot_S_joint(stan_list[[1]], add_km = TRUE,
                           annot_cf = FALSE)
gg_12[[2]] <- plot_S_joint(stan_list[[3]], add_km = TRUE,
                           annot_cf = FALSE)
ghier12 <- do.call("grid_arrange_shared_legend",
                   c(gg_12, nrow = 2, ncol = 1))

ggsave(ghier12, filename = glue::glue("plots/plot_S_grid_cf_hier_cpt_12m.png"),
       units = "in", width = fig_width, height = fig_height, dpi = 300)


gg_30 <- list()

gg_30[[1]] <- plot_S_joint(stan_list[[2]], add_km = TRUE,
                           annot_cf = FALSE)
gg_30[[2]] <- plot_S_joint(stan_list[[4]], add_km = TRUE,
                           annot_cf = FALSE)
ghier30 <- do.call("grid_arrange_shared_legend",
                   c(gg_30, nrow = 2, ncol = 1))

ggsave(ghier30, filename = glue::glue("plots/plot_S_grid_cf_hier_cpt_30m.png"),
       units = "in", width = fig_width, height = fig_height, dpi = 300)


#################
# forest plots

forest_hier <-
  cf_forest_cutpoint(folder = "data/dbl_cut/hier/", save_name = c("_30", "_12", "_100"))

forest_hier

ggsave(forest_hier, filename = glue::glue("plots/forest_plot_cf_hier_cpt.png"),
       units = "in", width = 16, height = 14, dpi = 300)


############
# separate #
############

###################
# survival curves

folder <- here::here("data/dbl_cut/separate/")

# read in data
filenames <- list.files(folder, full.names = TRUE)
keep_files <- grepl(pattern = "bmcm_stan_[a-z]+_[a-z]+_\\d+.Rds", filenames)
filenames <- filenames[keep_files]
stan_list <- map(filenames, readRDS)
names(stan_list) <-
  gsub(".Rds", "", list.files(folder, full.names = FALSE)[keep_files])

###################
# survival curves

gg_12 <- list()

gg_12[[1]] <- plot_S_joint(stan_list[[1]], add_km = TRUE,
                           annot_cf = FALSE)
gg_12[[2]] <- plot_S_joint(stan_list[[3]], add_km = TRUE,
                           annot_cf = FALSE)
gsep12 <- do.call("grid_arrange_shared_legend",
                  c(gg_12, nrow = 2, ncol = 1))


ggsave(gsep12, filename = glue::glue("plots/plot_S_grid_cf_sep_cpt_12m.png"),
       units = "in", width = fig_width, height = fig_height, dpi = 300)


gg_30 <- list()

gg_30[[1]] <- plot_S_joint(stan_list[[2]], add_km = TRUE,
                           annot_cf = FALSE)
gg_30[[2]] <- plot_S_joint(stan_list[[4]], add_km = TRUE,
                           annot_cf = FALSE)
gsep30 <- do.call("grid_arrange_shared_legend",
                  c(gg_30, nrow = 2, ncol = 1))


ggsave(gsep30, filename = glue::glue("plots/plot_S_grid_cf_sep_cpt_30m.png"),
       units = "in", width = fig_width, height = fig_height, dpi = 300)


################
# forest plots

forest_sep <-
  cf_forest_cutpoint(folder = "data/dbl_cut/separate/", save_name = c("_30", "_12"))
forest_sep

ggsave(forest_sep, filename = glue::glue("plots/forest_plot_cf_sep_cpt.png"),
       units = "in", width = 16, height = 14, dpi = 300)

