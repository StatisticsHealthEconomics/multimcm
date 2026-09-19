# Chronic granulomatous disease (CGD) analysis

``` r

library(purrr)
library(reshape2)
library(dplyr)
library(rstan)
library(dplyr)
library(glue)
library(abind)
library(survival)
library(multimcm)

library(multimcm)

# rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)
```

## Introduction

The analysis is a comparison with a frequentist analysis in Lai and Yau
(2009).

## Data

The data is chronic granulomatous disease (CGD) data available as part
of the `{frailtyHL}` package from Fleming and Harrington (1991). From
the package help documents

> The CGD data set in Fleming and Harrington (1991) is from a
> placebo-controlled randomized trial of gamma interferon in chronic
> granulomatous disease. In total, 128 patients from 13 hospitals were
> followed for about 1 year. The number of patients per hospital ranged
> from 4 to 26. Each patient may experience more than one infection. The
> survival times (times-to-event) are the times between recurrent CGD
> infections on each patient (i.e. gap times). Censoring occurred at the
> last observation for all patients, except one, who experienced a
> serious infection on the date he left the study.

``` r

data(cgd)
head(cgd)
```

Create a new variable for the duration until the event.

``` r

cgd <- mutate(cgd, time = tstop - tstart)
# center_id = as.numeric(as.factor(center))
```

The data set has repeat measurements for recurrent infections which we
currently have not implemented in
[multimcm](https://github.com/StatisticsHealthEconomics/multimcm) so for
simplicity we’ll remove some of the data in the initial analyses. In Lai
and Yau (2009) hospital-level random effects are assigned to the cured
probability part, and both patient- and hospital-level random effects
are assigned to hazard function part.
[multimcm](https://github.com/StatisticsHealthEconomics/multimcm)
currently only applies hierarchical structure to the cure fraction and
not to the separate hazards.

Take the first event for each individual by start time.

``` r

cgd_first <-
  cgd |> 
  group_by(id) |> 
  arrange(tstart) |> 
  dplyr::filter(row_number() == 1)
```

The full data set also has 13 hospitals/centers so select a smaller
number:

| Center               | Country       |
|----------------------|---------------|
| Amsterdam            | Netherlands   |
| NIH                  | United States |
| University of Zurich | Switzerland   |
| Scripps Institute    | United States |

Drop any levels which only appeared in the data we’ve just removed and
convert the variables to numeric or factor for unique and categorical
data, respectively.

``` r

cgd_center <- cgd_first |>
  dplyr::filter(center %in% c("Amsterdam", "NIH", "Univ. of Zurich", "Scripps Institute")) |>
  droplevels() |> 
  mutate(center_id = as.integer(center),
         hos.cat_id = as.integer(hos.cat),
         sex_id = as.factor(as.integer(sex)))
```

Next, append the background hazard rate by age, sex and country. These
rare are available from the [WHO
website](https://www.who.int/data/gho/data/themes/topics/topic-details/GHO/healthy-life-expectancy-(hale))
WHO (2020). We will read from our own local package `{bgfscure}` for
simplicity. Next, we need to harmonise the country names and their
associated cities. Then we join the data with the background mortality
data according to age, sex and country.

``` r

# WHO background mortality data
load(here::here("data/bg.mortality.RData"))  # bg.mortality

cgd_center <-
  cgd_center |>
  tidyr::separate(hos.cat, c("country", "hospital"),
                  remove = FALSE) |> 
  mutate(country = toupper(country),
         age_event = round(age + time/365.25, 0),   # convert from days to years
         country = ifelse(center == "Amsterdam", "NETHERLANDS",
                          ifelse(center == "Univ. of Zurich", "SWITZERLAND",
                                 ifelse(center == "Copenhagen", "DENMARK", country)))) |> 
  select(-hospital)

# harmonise fields
bg.mortality <- bg.mortality |> 
  mutate(ACOUNTRY = toupper(ACOUNTRY),
         ACOUNTRY = ifelse(ACOUNTRY == "UNITED STATES", "US", ACOUNTRY),
         SEX = ifelse(SEX == "M", "male", "female"),
         rate = ifelse(rate == 0, 1e-10, rate)) |>  # replace so >0
  rename(country = ACOUNTRY,
         sex = SEX,
         age_event = Age)

input_data <- merge(cgd_center, bg.mortality,
                    by = c("age_event", "sex", "country"), sort = FALSE)
```

Creating `input_data` is the last step in the data wrangling. We can now
proceed to the mixture cure modelling analysis.

## Analysis

#### By center

No covariates in the latent model and the incidence (cure) model as \$\$
T \sim \text{Exp}(\lambda)\\ \pi_i = \text{logit}^{-1}(\alpha +
\beta\_{\text{treat}\[i\]} + \gamma\_{\text{center}\[i\]})\\
\gamma\_{\text{center}\[i\]} \sim N(\mu\_{\text{center}},
\sigma^2\_{\text{center}}) \$\$

``` r

out <-
  bmcm_stan(
    input_data = input_data,
    formula = "Surv(time=time, event=status) ~ 1",
    cureformula = "~ treat + (1 | center_id)",
    family_latent = "exponential",
    centre_coefs = TRUE,
    bg_model = "bg_fixed",
    bg_varname = "rate",
    bg_hr = 1,
    t_max = 400,
    use_cmdstanr = TRUE)
```

Alternatively, we can pass in a *precompiled* model, rather than have
the model compiled at the time of fitting.

``` r

model_nm <- "precompiled_stan_model"

# create the precompiled model and save as .RDS file
precompile_bmcm_model(input_data = input_data,
                      cureformula =  "~ treat + (1 | center_id)",
                      family_latent = "exponential",
                      model_name = model_nm,
                      use_cmdstanr = TRUE)
```

Now, load back in the compiled model and fit as before.

``` r

model_path <- glue::glue("{system.file('stan', package = 'multimcm')}/{model_nm}.exe")

out_precompiled <-
  bmcm_stan(
    input_data = input_data,
    formula = "Surv(time=time, event=status) ~ 1",
    cureformula = "~ treat + (1 | center_id)",
    family_latent = "exponential",
    centre_coefs = TRUE,
    bg_model = "bg_fixed",
    bg_varname = "rate",
    bg_hr = 1,
    t_max = 400,
    precompiled_model_path = model_path,
    use_cmdstanr = TRUE)
```

We can do the same but using a `cmdstanr` model, rather than an `rstan`
model. This has some benefits in terms of speed, reliability and memory
usage. Firstly for a non-compiled model, set `use_cmdstanr = TRUE`
argument.

``` r

out <-
  bmcm_stan(
    input_data = input_data,
    formula = "Surv(time=time, event=status) ~ 1",
    cureformula = "~ treat + (1 | center_id)",
    family_latent = "exponential",
    centre_coefs = TRUE,
    bg_model = "bg_fixed",
    bg_varname = "rate",
    bg_hr = 1,
    t_max = 400,
    use_cmdstanr = TRUE)

# so that plotting functions work later
# convert cmdstanr object to rstan stanfit format

stanfit <- brms::read_csv_as_stanfit(out$output$output_files())
```

Secondly, we can pass in a *precompiled* `cmdstanr` model. This saves an
`.exe` file in the `stan` folder.

``` r

model_nm <- "precompiled_stan_model"

precompile_bmcm_model(input_data = input_data,
                      cureformula =  "~ treat + (1 | center_id)",
                      family_latent = "exponential",
                      model_name = model_nm,
                      use_cmdstanr = TRUE)
```

and load back in the compiled model and fit as before.

``` r

model_path <- glue::glue("{system.file('stan', package = 'multimcm')}/{model_nm}.exe")

out_precompiled <-
  bmcm_stan(
    input_data = input_data,
    formula = "Surv(time=time, event=status) ~ 1",
    cureformula = "~ treat + (1 | center_id)",
    family_latent = "exponential",
    centre_coefs = TRUE,
    bg_model = "bg_fixed",
    bg_varname = "rate",
    bg_hr = 1,
    t_max = 400,
    precompiled_model_path = model_path,
    use_cmdstanr = TRUE)

##TODO:
# out_precompiled$output <-
#   rstan::read_stan_csv(out_precompiled$output$output_files())
```

##### Separate models

We can also fit the end-points separately by changing the `cureformula`
argument to a formula without a random effect term
`~ treat + center_id`.

``` r

stan_model <- 
  precompile_bmcm_model(input_data = input_data,
                        cureformula =  "~ treat + center_id",
                        family_latent = "exponential",
                        model_name = "precompiled_stan_model",
                        use_cmdstanr = TRUE)

model_path <- stan_model$exe_file()

out_precompiled_separate <-
  bmcm_stan(
    input_data = input_data,
    formula = "Surv(time=time, event=status) ~ 1",
    cureformula = "~ treat + center_id",
    family_latent = "exponential",
    centre_coefs = TRUE,
    bg_model = "bg_fixed",
    bg_varname = "rate",
    bg_hr = 1,
    t_max = 400,
    precompiled_model_path = model_path,
    use_cmdstanr = TRUE)
```

#### By hospital category

No covariates in the latent model and the incidence (cure) model as \$\$
T \sim \text{Exp}(\lambda)\\ \pi_i = \text{logit}^{-1}(\alpha +
\beta\_{\text{treat}\[i\]} + \gamma\_{\text{cat}\[i\]})\\
\gamma\_{\text{cat}\[i\]} \sim N(\mu\_{\text{cat}},
\sigma^2\_{\text{cat}}) \$\$

``` r

out_hos.cat <-
  bmcm_stan(
    input_data = input_data,
    formula = "Surv(time=time, event=status) ~ 1",
    cureformula = "~ treat + (1 | hos.cat_id)",
    family_latent = "exponential",
    centre_coefs = TRUE,
    bg_model = "bg_fixed",
    bg_varname = "rate",
    bg_hr = 1,
    t_max = 365,
    use_cmdstanr = TRUE)
```

#### With sex covariate in latent model

No covariates in the latent model and the incidence (cure) model as \$\$
T \sim \text{Exp}(\lambda_i)\\ \log(\lambda_i) = \alpha\_{\lambda} +
\beta\_{\text{sex}\[i\]}\\ \pi_i = \text{logit}^{-1}(\alpha\_{\pi} +
\beta\_{\text{treat}\[i\]} + \gamma\_{\text{center}\[i\]})\\
\gamma\_{\text{center}\[i\]} \sim N(\mu\_{\text{center}},
\sigma^2\_{\text{center}}) \$\$

``` r

out_sex <-
  bmcm_stan(
    input_data = input_data,
    formula = "Surv(time=time, event=status) ~ sex",
    cureformula = "~ treat + (1 | hos.cat_id)",
    family_latent = "exponential",
    bg_model = "bg_fixed",
    bg_varname = "rate",
    bg_hr = 1,
    t_max = 365,
    use_cmdstanr = TRUE)
```

### Plots

After fitting the models, we can plot the survival curves for each.

``` r

library(ggplot2)
```

#### Main models

Hierarchy by `center_id`.

``` r

gg <- plot_S_joint(out,
                   add_km = TRUE,
                   annot_cf = FALSE)
gg + xlim(0,365) + facet_wrap(~endpoint)
```

Hierarchy by `center_id` with precompiled stan model. Should be the
same!

Separate models, i.e fixed effect for `center_id`. TODO: THIS DOESNT
LOOK RIGHT

#### By center

``` r

gg2 <- plot_S_joint(out_hos.cat,
                    add_km = TRUE,
                    annot_cf = FALSE)
gg2 + xlim(0,365) + facet_wrap(~endpoint)
```

``` r

gg3 <- plot_S_joint(out_sex,
                    add_km = TRUE,
                    annot_cf = FALSE)
gg3 + xlim(0,365) + facet_wrap(~endpoint)
```

### References

Fleming, Thomas R., and David P. Harrington. 1991. *Counting Processes
and Survival Analysis*. Wiley.

Lai, Xin, and Kelvin K. W. Yau. 2009. “Multilevel Mixture Cure Models
with Random Effects.” *Biometrical Journal* 51 (3): 456–66.
<https://doi.org/10.1002/bimj.200800222>.

WHO. 2020. *Life tables by country*.
<http://apps.who.int/gho/data/?theme=main&vid=61160>.
