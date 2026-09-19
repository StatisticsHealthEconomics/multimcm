# Cure fraction forest plot using cut-point Stan output

Generates a forest plot of the cure fractions for different treatments
using Stan model outputs fitted with specific time cut-points. Useful
for comparing cure probabilities across interventions.

## Usage

``` r
cf_forest_cutpoint(
  distns = list(c("exp", "exp"), c("lognormal", "lognormal")),
  folder = NA,
  save_name = c("_12", "_30", "_100"),
  is_hier = TRUE
)
```

## Arguments

- distns:

  A list or character vector of distribution names used in the model
  fits.

- folder:

  A string specifying the directory path containing the saved model
  output files.

- save_name:

  A string or vector of text strings appended to the file names (e.g.,
  specific cut-points).

- is_hier:

  Logical. Is it a hierarchical model? If `TRUE`, it accounts for a
  global grouping parameter.

## Value

A `ggplot2` object showing the cure fraction forest plot.

## Examples

``` r
if (FALSE) { # \dontrun{
 # independent model
 fp_sep <- cf_forest_cutpoint(folder = "data/dbl_cut/separate", save_name = c("_30", "_12", "_100"))
 #ggsave(fp_sep, filename = "plots/forest_plot_cf_sep_cpt.png",
 #       dpi = 640, width = 16, height = 14)

 # hierarchical model
 fp_hier <- cf_forest_cutpoint(folder = "data/dbl_cut/hier", save_name = c("_30", "_12", "_100"))
 #ggsave(fp_hier, filename = "plots/forest_plot_cf_hier_cpt.png",
 #       dpi = 640, width = 16, height = 14)
} # }
```
