# bmcm_stan

Jointly estimates all treatments and generates Stan code for Bayesian
relative mixture cure modeling. It supports various latent and cure
model configurations and allows the use of precompiled models or
on-the-fly compilation.

## Usage

``` r
bmcm_stan(
  input_data,
  formula,
  cureformula = ~1,
  family_latent = "exponential",
  prior_latent = NA,
  prior_cure = list(),
  centre_coefs = FALSE,
  bg_model = c("bg_distn", "bg_fixed"),
  bg_varname = "bg_rate",
  bg_hr = 1,
  t_max = 70,
  save_stan_code = FALSE,
  read_stan_code = FALSE,
  precompiled_model_path = NA,
  use_cmdstanr = FALSE,
  ...
)
```

## Arguments

- input_data:

  A long-format data frame containing the input data.

- formula:

  An R formula object specifying the latent model component.

- cureformula:

  An R formula object specifying the cure fraction model component.
  Default is `~ 1`.

- family_latent:

  A string or vector specifying the distribution(s) for the latent model
  component. Supported values are `"exp"`, `"weibull"`, `"gompertz"`,
  `"lognormal"`, `"loglogistic"`, and `"gengamma"`.

- prior_latent:

  An optional prior specification for the latent model component.
  Default is `NA`.

- prior_cure:

  An optional list specifying priors for the cure fraction model
  component. Default is an empty list.

- centre_coefs:

  Logical. If `TRUE`, coefficients will be centered. Default is `FALSE`.

- bg_model:

  A string specifying the background model type. Supported values are
  `"bg_distn"` and `"bg_fixed"`.

- bg_varname:

  A string specifying the background variable name in `input_data`.
  Default is `"bg_rate"`.

- bg_hr:

  A numeric value for the background hazard ratio adjustment. Default is
  `1`.

- t_max:

  A numeric value specifying the maximum time horizon for the model.
  Default is `70`.

- save_stan_code:

  Logical. If `TRUE`, saves the Stan model code to a file. Default is
  `FALSE`.

- read_stan_code:

  Logical. If `TRUE`, reads the Stan model code from a file instead of
  generating it dynamically. Default is `FALSE`.

- precompiled_model_path:

  A string specifying the path to a precompiled model. Default is `NA`.

- use_cmdstanr:

  Logical. If `TRUE`, the `cmdstanr` package is used to fit the model.
  Default is `FALSE`.

- ...:

  Additional parameters to pass to the Stan sampler.

## Value

A list of class `bmcm` containing the Stan model output, MCMC
parameters, function call, distributions used, model inputs, input data,
and formulas for the cure and latent components.

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
  bg_varname = "rate",
  t_max = 400
)
} # }
```
