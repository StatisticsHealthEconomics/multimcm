# Prepare Stan data in latent model

Data specific to end type for Stan input.

## Usage

``` r
prep_latent_data(
  input_data,
  formula_cure,
  formula_latent,
  event_type,
  centre_coefs = FALSE,
  suffix = TRUE
)
```

## Arguments

- input_data:

  Data frame

- formula_cure:

  parsed formula

- formula_latent:

  parsed formula

- event_type:

  cluster/group

- centre_coefs:

  Logical

- suffix:

  Logical

## Value

List consisting of: sample size, times, censoring indicator, number of
covariates, covariates
