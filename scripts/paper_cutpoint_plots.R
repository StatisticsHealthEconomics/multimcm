
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

drug_names <- c('Ipilimumab', 'Nivolumab', 'Ipilimumab + Nivolumab', 'Cured', 'Uncured')

modify_labels <-
  list(scale_fill_discrete(labels = drug_names),
       scale_color_discrete(labels = drug_names))

modify_facet_titles <- facet_grid(~ endpoint, labeller = labeller(endpoint = c("1" = "OS", "2" = "PFS")))


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

gg_hier_12 <- list()

gg_hier_12[[1]] <- plot_S_joint(stan_list$bmcm_stan_exp_exp_12, add_km = TRUE,
                                annot_cf = FALSE) + modify_labels + modify_facet_titles

gg_hier_12[[2]] <- plot_S_joint(stan_list$bmcm_stan_lognormal_lognormal_12, add_km = TRUE,
                                annot_cf = FALSE) + modify_labels + modify_facet_titles

grid_hier_12 <- do.call("grid_arrange_shared_legend",
                        c(gg_hier_12, nrow = 2, ncol = 1))

ggsave(grid_hier_12, filename = glue::glue("plots/plot_S_grid_cf_hier_cpt_12m.png"),
       units = "in", width = fig_width, height = fig_height, dpi = 300)


gg_hier_30 <- list()

gg_hier_30[[1]] <- plot_S_joint(stan_list$bmcm_stan_exp_exp_30, add_km = TRUE,
                                annot_cf = FALSE) + modify_labels + modify_facet_titles
gg_hier_30[[2]] <- plot_S_joint(stan_list$bmcm_stan_lognormal_lognormal_30, add_km = TRUE,
                                annot_cf = FALSE) + modify_labels + modify_facet_titles
grid_hier_30 <- do.call("grid_arrange_shared_legend",
                        c(gg_hier_30, nrow = 2, ncol = 1))

ggsave(grid_hier_30, filename = glue::glue("plots/plot_S_grid_cf_hier_cpt_30m.png"),
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

gg_sep_12 <- list()

gg_sep_12[[1]] <- plot_S_joint(stan_list$bmcm_stan_exp_exp_12, add_km = TRUE,
                               annot_cf = FALSE) + modify_labels + modify_facet_titles
gg_sep_12[[2]] <- plot_S_joint(stan_list$bmcm_stan_lognormal_lognormal_12, add_km = TRUE,
                               annot_cf = FALSE) + modify_labels + modify_facet_titles
grid_sep_12 <- do.call("grid_arrange_shared_legend",
                       c(gg_sep_12, nrow = 2, ncol = 1))

ggsave(grid_sep_12, filename = glue::glue("plots/plot_S_grid_cf_sep_cpt_12m.png"),
       units = "in", width = fig_width, height = fig_height, dpi = 300)


gg_sep_30 <- list()

gg_sep_30[[1]] <- plot_S_joint(stan_list$bmcm_stan_exp_exp_30, add_km = TRUE,
                               annot_cf = FALSE) + modify_labels + modify_facet_titles
gg_sep_30[[2]] <- plot_S_joint(stan_list$bmcm_stan_lognormal_lognormal_30, add_km = TRUE,
                               annot_cf = FALSE) + modify_labels + modify_facet_titles
grid_sep_30 <- do.call("grid_arrange_shared_legend",
                       c(gg_sep_30, nrow = 2, ncol = 1))

ggsave(grid_sep_30, filename = glue::glue("plots/plot_S_grid_cf_sep_cpt_30m.png"),
       units = "in", width = fig_width, height = fig_height, dpi = 300)


################
# forest plots

forest_sep <-
  cf_forest_cutpoint(folder = "data/dbl_cut/separate/", save_name = c("_30", "_12", "_100"))
forest_sep

ggsave(forest_sep, filename = glue::glue("plots/forest_plot_cf_sep_cpt.png"),
       units = "in", width = 16, height = 14, dpi = 300)


###################
# exponential only

grid_exp_12 <- do.call("grid_arrange_shared_legend",
                       c(list(gg_sep_12[[1]], gg_hier_12[[1]]), nrow = 2, ncol = 1))

ggsave(grid_exp_12, filename = glue::glue("plots/plot_S_grid_cf_exponential_cpt_12.png"),
       units = "in", width = fig_width, height = fig_height, dpi = 300)

grid_exp_30 <- do.call("grid_arrange_shared_legend",
                       c(list(gg_sep_30[[1]], gg_hier_30[[1]]), nrow = 2, ncol = 1))

ggsave(grid_exp_30, filename = glue::glue("plots/plot_S_grid_cf_exponential_cpt_30.png"),
       units = "in", width = fig_width, height = fig_height, dpi = 300)

