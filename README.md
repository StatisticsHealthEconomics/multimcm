
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multimcm: Multilevel mixture cure modelling in Stan <img src='man/figures/hexbadge.png' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/StatisticsHealthEconomics/rstanbmcm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/StatisticsHealthEconomics/rstanbmcm/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of multimcm is to jointly model two event time distributions
(e.g. time to disease progression (PFS) and time to death (OS)) within a
Bayesian relative survival mixture cure model framework, using the Stan
engine called from R.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("StatisticsHealthEconomics/multimcm")
```

## Motivation

Mixture cure models are increasingly popular in health problems and in
particular in oncology. A Bayesian paradigm allows the explicitly
incorporation of uncertainties and principled synthesis of prior
knowledge such as via expert elicitation or previous trials. By
extending current methods to account for the dependence between event
times we leverage additional information to make better inferences and
decisions.


### Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
