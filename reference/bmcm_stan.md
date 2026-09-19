# bmcm_stan

Jointly models two or more event time distributions within a Bayesian
relative survival mixture cure model framework. The function generates
custom Stan code on-the-fly (or uses precompiled models) and estimates
treatment effects simultaneously for both the cure fraction and the
latent survival distributions. It allows for complex hierarchical
structures in the cure fraction, background mortality adjustment, and
multiple parametric families for the latent survival times (e.g.,
exponential, Weibull, Gompertz).

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

An object of class `bmcm`, which is a list containing the following
components:

- `output`: The fitted Stan model object (either a `stanfit` object from
  rstan or a `CmdStanMCMC` object from cmdstanr).

- `mcmc_params`: A list of the MCMC sampling parameters used (e.g.,
  iterations, warmup, chains).

- `call`: The matched call to the function.

- `distns`: A character vector of the latent survival distributions
  used.

- `inputs`: A list of the formatted data inputs passed directly to the
  Stan model.

- `input_data`: The original `input_data` data frame provided.

- `formula`: A list containing the parsed `cure` and `latent` model
  formulas.

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
