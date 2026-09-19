# Plot survival curves for joint model and all treatments

Generates a plot of the survival curves for the joint relative survival
mixture cure model, overlaying the predicted survival probabilities and
(optionally) the original Kaplan-Meier curves for all treatments.

## Usage

``` r
plot_S_joint(
  bmcm_out,
  facet = TRUE,
  annot_cf = FALSE,
  add_km = FALSE,
  add_marks = TRUE,
  ...
)
```

## Arguments

- bmcm_out:

  bmcm class output list, as returned by
  [`bmcm_stan()`](https://statisticshealtheconomics.github.io/multimcm/reference/bmcm_stan.md)

- facet:

  Logical. Should the plots for each endpoint be separated into facets?
  Default is `TRUE`.

- annot_cf:

  Logical. Annotate the plot with the cure fractions? Default is
  `FALSE`.

- add_km:

  Logical. Include an overlaid Kaplan-Meier curve of the raw data?
  Default is `FALSE`.

- add_marks:

  Logical. Include Kaplan-Meier censoring marks? Default is `TRUE`.

- ...:

  Additional parameters passed to the plotting function.

## Value

A `ggplot2` object showing the survival curves.

## Details

Use results of running Stan with
[`bmcm_stan()`](https://statisticshealtheconomics.github.io/multimcm/reference/bmcm_stan.md)
relative survival joint mixture cure model.

## Examples

``` r
if (FALSE) { # \dontrun{
data("surv_input_data", package = "multimcm")

out <- bmcm_stan(
  input_data = surv_input_data,
  formula = "Surv(time=os, event=os_event) ~ 1",
  cureformula = "~ TRTA + (1 | center_id)",
  family_latent = "exponential",
  bg_model = "bg_fixed",
  bg_varname = "rate"
)

# Generate the survival plot, facetting by endpoint
surv_plot <- plot_S_joint(out, facet = TRUE, add_km = TRUE)
print(surv_plot)
} # }
```
