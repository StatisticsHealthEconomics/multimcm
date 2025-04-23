
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multimcm: Multilevel mixture cure modelling in Stan <img src='man/figures/hexbadge.png' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/StatisticsHealthEconomics/rstanbmcm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/StatisticsHealthEconomics/rstanbmcm/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of multimcm is to jointly model multiple event time distributions
(e.g. time to disease progression (PFS) and time to death (OS)) within a
Bayesian relative survival mixture cure model framework, using the Stan
engine called from R.

This package is particularly useful in health economics and oncology research, enabling users to model dependencies between event times and make better-informed inferences and decisions.

---

## Features

- **Bayesian Mixture Cure Modeling**: Analyze event time distributions with a focus on cure modeling.
- **Joint Modeling of Event Times**: Account for dependencies between progression-free survival (PFS) and overall survival (OS).
- **Stan Integration**: Leverage the power of Stan for Bayesian inference.
- **Health Economics Applications**: Designed for use in oncology and other domains requiring relative survival analysis.
- **R Interface**: Easy-to-use R-based interface for statistical modeling.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")  # if you don't already have it
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

## Contributing

Contributions to `multimcm` are welcome! Please adhere to the [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html) when contributing to this project.

To contribute:
1. Fork the repository.
2. Create a feature branch (`git checkout -b feature-name`).
3. Commit your changes (`git commit -m "Add feature"`).
4. Push to the branch (`git push origin feature-name`).
5. Open a pull request.

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.

---
