// log-normal mixture cure model
// relative survival

functions {
#include /include/distributions.stan
}

// input data ----
data {
  int<lower=0> n;             // number of observations
  vector[n] t;                // observed times
  vector[n] d;                // censoring indicator (1 = observed, 0 = censored)
  int<lower = 0> H;           // number of covariates
  matrix[n,H] X;              // matrix of covariates (with n rows and H columns)

  real<lower=0> a_sd;
  real<lower=0> b_sd;

  vector[H] mu_0;
  vector<lower=0> [H] sigma_0;

  vector[H] mu_bg;
  vector<lower=0> [H] sigma_bg;

  real<lower=0> a_cf;             // cure fraction ~ Beta(a,b)
  real<lower=0> b_cf;

  int<lower=0> t_max;
}

parameters {
  vector[H] beta0;         // coefficients in linear predictor (including intercept)
  vector[H] beta_bg;
  real<lower=0> sd_0;
  real<lower=0, upper=1> curefrac;
}

transformed parameters {
  vector[n] linpred0;
  vector[n] linpred_bg;
  vector[n] lambda_bg;
  vector[n] mean_0;

  linpred0 = X*beta0;
  linpred_bg = X*beta_bg;

  lambda_bg = exp(linpred_bg);
  mean_0 = linpred0;
}

model {
  beta0 ~ normal(mu_0, sigma_0);
  beta_bg ~ normal(mu_bg, sigma_bg);

  // not dependent on X or tranformed
  sd_0 ~ gamma(a_sd, b_sd);

  curefrac ~ beta(a_cf, b_cf);

  for (i in 1:n) {

    target += log_sum_exp(log(curefrac) +
                surv_exp_lpdf(t[i] | d[i], lambda_bg[i]),
                log1m(curefrac) +
                joint_exp_lognormal_lpdf(t[i] | d[i], mean_0[i], sd_0, lambda_bg[i]));
  }
}

generated quantities {
  real rate0;
  real rate_bg;
  vector[t_max] S_bg;
  vector[t_max] S0;
  vector[t_max] S_pred;
  real mean_lp;

  mean_lp = beta0[1];
  rate_bg = exp(beta_bg[1]);

  for (i in 1:t_max) {
    S_bg[i] = exp_Surv(i, rate_bg);
    S0[i] = exp_lognormal_Surv(i, mean_lp, sd_0, rate_bg);
    S_pred[i] = curefrac*S_bg[i] + (1 - curefrac)*S0[i];
  }
}

