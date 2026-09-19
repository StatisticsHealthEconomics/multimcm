# Cure fraction forest plot using Stan output

Generates a forest plot displaying the estimated cure fractions across
all treatments derived from the joint relative survival mixture cure
model output.

## Usage

``` r
cf_forest_plot(folder = "data/")
```

## Arguments

- folder:

  A string specifying the directory path containing the saved Stan model
  output files.

## Value

A `ggplot2` object showing the cure fraction forest plot.

## Examples

``` r
if (FALSE) { # \dontrun{
 # independent model
 fp_sep <- cf_forest_plot("data/separate/")
 #ggsave(fp_sep, filename = "plots/forest_plot_sep_multimcm.png", dpi = 640, width = 12, height = 8)

 # hierarchical model
 fp_hier <- cf_forest_plot("data/")
 #ggsave(fp_hier, filename = "plots/forest_plot_multimcm.png", dpi = 640, width = 12, height = 8)
} # }
```
