# Prepare posterior survival data for plotting

Prepare posterior survival data for plotting

## Usage

``` r
prep_S_data(
  stan_extract,
  event_type = NA,
  CI_probs = c(0.025, 0.5, 0.975),
  tx_idx = NA
)
```

## Arguments

- stan_extract:

  List of stan output

- event_type:

  String specifying event type

- CI_probs:

  Numeric vector of credible interval probabilities

- tx_idx:

  Numeric vector of treatment indices

## Value

dataframe
