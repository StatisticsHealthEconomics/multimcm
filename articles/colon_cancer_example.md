# Colon Cancer (cuRe) analysis

## Introduction

This vignette demonstrates how to use the
[multimcm](https://github.com/StatisticsHealthEconomics/multimcm)
package to fit a Bayesian relative survival mixture cure model using the
`colonDC` dataset from the [cuRe](https://github.com/LasseHjort/cuRe)
package.

## Data

The `colonDC` dataset contains individual baseline and follow-up data
for over 15,000 colon cancer patients. It is frequently used for
demonstrating parametric cure model estimation in relative survival
frameworks.

``` r

library(dplyr)
library(rstan)
library(survival)
library(multimcm)
# install.packages("cuRe")
library(cuRe)

options(mc.cores = parallel::detectCores() - 1)
```

``` r

data("colonDC", package = "cuRe")
head(colonDC)
```

We will prepare the dataset by defining the survival time variable
`time`, the event indicator `status`, and formatting the covariates we
want to include in our model. In this example, we use `FUyear` as the
survival time and the patient’s cancer `stage` as a hierarchical random
effect.

We also append a constant background mortality rate for simplicity. In a
real application, you would merge population-level background hazards
(e.g., from WHO life tables) based on age, sex, and diagnosis year.

``` r

input_data <- colonDC |>
  mutate(
    time = FUyear,
    status = status, # 0 = alive, 1 = dead
    stage_id = as.integer(as.factor(stage)),
    sex_id = as.factor(sex),
    rate = 0.02 # illustrative background mortality rate
  ) |> 
  dplyr::filter(!is.na(stage), !is.na(time), !is.na(status)) |>
  droplevels()
```

## Analysis

We will define the incidence (cure) model with a fixed effect for sex
and a random effect for the clinical stage. The latent survival model
will be exponential with no covariates.

\$\$ T \sim \text{Exp}(\lambda)\\ \pi_i = \text{logit}^{-1}(\alpha +
\beta\_{\text{sex}\[i\]} + \gamma\_{\text{stage}\[i\]})\\
\gamma\_{\text{stage}\[i\]} \sim N(\mu\_{\text{stage}},
\sigma^2\_{\text{stage}}) \$\$

``` r

out <-
  bmcm_stan(
    input_data = input_data,
    formula = "Surv(time=time, event=status) ~ 1",
    cureformula = "~ sex + (1 | stage_id)",
    family_latent = "exponential",
    centre_coefs = TRUE,
    bg_model = "bg_fixed",
    bg_varname = "rate",
    bg_hr = 1,
    t_max = 20, # up to 20 years follow up
    use_cmdstanr = TRUE
  )
```

#### Precompiling the Model

Alternatively, we can precompile the Stan model to save time across
multiple runs.

``` r

model_nm <- "colon_stan_model"

precompile_bmcm_model(
  input_data = input_data,
  cureformula = "~ sex + (1 | stage_id)",
  family_latent = "exponential",
  model_name = model_nm,
  use_cmdstanr = TRUE
)

model_path <- glue::glue("{system.file('stan', package = 'multimcm')}/{model_nm}.exe")

out_precompiled <-
  bmcm_stan(
    input_data = input_data,
    formula = "Surv(time=time, event=status) ~ 1",
    cureformula = "~ sex + (1 | stage_id)",
    family_latent = "exponential",
    centre_coefs = TRUE,
    bg_model = "bg_fixed",
    bg_varname = "rate",
    bg_hr = 1,
    t_max = 20,
    precompiled_model_path = model_path,
    use_cmdstanr = TRUE
  )
```

## Plots

After fitting the model, we can visualize the estimated survival and
relative survival curves.

``` r

library(ggplot2)

gg <- plot_S_joint(out, add_km = TRUE, annot_cf = FALSE)
gg + xlim(0, 15) + facet_wrap(~endpoint)
```
