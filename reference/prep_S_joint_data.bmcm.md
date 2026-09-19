# Prepare data for survival plot

An internal/helper function used to extract and format survival
predictions from a fitted `bmcm` model. It generates a long-format data
frame containing the mean survival estimates and their credible
intervals across time points.

## Usage

``` r
# S3 method for class 'bmcm'
prep_S_joint_data(bmcm_out, ...)
```

## Arguments

- bmcm_out:

  Output list from the
  [`bmcm_stan()`](https://statisticshealtheconomics.github.io/multimcm/reference/bmcm_stan.md)
  function.

- ...:

  Additional arguments passed to methods.

## Value

A data frame containing the summarized survival predictions.

## Examples

``` r
if (FALSE) { # \dontrun{
data("surv_input_data", package = "multimcm")
out <- bmcm_stan(
  input_data = surv_input_data,
  formula = "Surv(time=os, event=os_event) ~ 1",
  cureformula = "~ TRTA",
  family_latent = "exponential",
  bg_model = "bg_fixed",
  bg_varname = "rate"
)
plot_data <- prep_S_joint_data(out)
head(plot_data)
} # }
```
