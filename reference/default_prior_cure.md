# Prepare shared parameters for Stan model

Model parameters for cure fraction, background and generated.

## Usage

``` r
default_prior_cure(formula_cure, prior_cure = list(), bg_model = 2)
```

## Arguments

- formula_cure:

  Result of `parse_formula`

- bg_model:

  Background model index:

  1.  exponential distribution

  2.  fixed point values from life-tables

## Value

list
