# Package index

## Model Fitting

Functions for compiling and fitting Bayesian mixture cure models.

- [`bmcm_stan()`](https://statisticshealtheconomics.github.io/multimcm/reference/bmcm_stan.md)
  : bmcm_stan
- [`precompile_bmcm_model()`](https://statisticshealtheconomics.github.io/multimcm/reference/precompile_bmcm_model.md)
  : Precompile bmcm model

## Visualization

Functions to create visual outputs of the fitted models.

- [`plot_S_joint()`](https://statisticshealtheconomics.github.io/multimcm/reference/plot_S_joint.md)
  : Plot survival curves for joint model and all treatments
- [`cf_forest_plot()`](https://statisticshealtheconomics.github.io/multimcm/reference/cf_forest_plot.md)
  : Cure fraction forest plot using Stan output
- [`cf_forest_cutpoint()`](https://statisticshealtheconomics.github.io/multimcm/reference/cf_forest_cutpoint.md)
  : Cure fraction forest plot using cut-point Stan output
- [`geom_kaplan_meier()`](https://statisticshealtheconomics.github.io/multimcm/reference/geom_kaplan_meier.md)
  : Geom for Kaplan-Meier ggplot
- [`grid_arrange_shared_legend()`](https://statisticshealtheconomics.github.io/multimcm/reference/grid_arrange_shared_legend.md)
  : Use single legend for grid of plots

## Data Preparation & Helpers

Internal and helper functions used to manipulate and prepare data.

- [`prep_S_joint_data()`](https://statisticshealtheconomics.github.io/multimcm/reference/prep_S_joint_data.md)
  : Prepare data for survival plot
- [`prep_S_joint_data(`*`<bmcm>`*`)`](https://statisticshealtheconomics.github.io/multimcm/reference/prep_S_joint_data.bmcm.md)
  : Prepare data for survival plot
- [`prep_S_joint_data(`*`<default>`*`)`](https://statisticshealtheconomics.github.io/multimcm/reference/prep_S_joint_data.default.md)
  : already extracted matrix
- [`prep_latent_data()`](https://statisticshealtheconomics.github.io/multimcm/reference/prep_latent_data.md)
  : Prepare Stan data in latent model
- [`prep_S_data()`](https://statisticshealtheconomics.github.io/multimcm/reference/prep_S_data.md)
  : Prepare posterior survival data for plotting
- [`prep_bg_data()`](https://statisticshealtheconomics.github.io/multimcm/reference/prep_bg_data.md)
  : Prepare background data
- [`default_prior_latent()`](https://statisticshealtheconomics.github.io/multimcm/reference/prep_stan_params.md)
  : Default prior latent variable
- [`create_stancode()`](https://statisticshealtheconomics.github.io/multimcm/reference/create_stancode.md)
  : Create Stan code from component parts
