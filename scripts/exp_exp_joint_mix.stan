// exponential mixture cure model
// joint relative survival

functions {
#include /include/distributions.stan
}

// input data ----
data {
  int<lower=0> n;             // number of observations
  vector[n] t_os;             // observed times
  vector[n] t_pfs;
  vector[n] d_os;             // censoring indicator (1 = observed, 0 = censored)
  vector[n] d_pfs;
  int<lower = 0> H;           // number of covariates
  matrix[n,H] X;              // matrix of covariates (with n rows and H columns)

  vector[H] mu_os;
  vector[H] mu_pfs;
  vector<lower=0> [H] sigma_os;
  vector<lower=0> [H] sigma_pfs;
  vector[H] mu_bg;
  vector<lower=0> [H] sigma_bg;
  real mu_joint;
  real<lower=0> [H] sigma_joint;

  real<lower=0, upper=1> curefrac;    // common between os, pfs

  int<lower=0> t_max;
}

parameters {
  vector[H] beta_os;       // coefficients in linear predictor (including intercept)
  vector[H] beta_pfs;
  vector[H] beta_bg;
}

transformed parameters {
  vector[n] lp_os;
  vector[n] lp_pfs;
  vector[n] lp_bg;
  vector[n] lambda_os;
  vector[n] lambda_pfs;
  vector[n] lambda_bg;

  lp_os = X*beta_os + beta_joint*(t_pfs - mean(lambda_pfs));
  lp_pfs = X*beta_pfs;
  lp_bg = X*beta_bg;

  // rate parameters
  lambda_os = exp(lp_os);
  lambda_pfs = exp(lp_pfs);
  lambda_bg = exp(lp_bg);     // background survival with uncertainty
}

model {
  beta_os ~ normal(mu_os, sigma_os);
  beta_pfs ~ normal(mu_pfs, sigma_pfs);
  beta_bg ~ normal(mu_bg, sigma_bg);
  beta_joint ~ normal(mu_joint, sigma_joint);

  for (i in 1:n) {
    target += log_sum_exp(log(curefrac) +
                surv_exp_lpdf(t[i] | d_os[i], lambda_bg[i]),
                log1m(curefrac) +
                surv_exp_lpdf(t[i] | d_os[i], lambda_bg[i] + lambda_os[i])) +
              log_sum_exp(log(curefrac) +
                surv_exp_lpdf(t[i] | d_pfs[i], lambda_bg[i]),
                log1m(curefrac) +
                surv_exp_lpdf(t[i] | d_pfs[i], lambda_bg[i] + lambda_pfs[i]));
  }
}

generated quantities {
  real rate_os;
  real rate_pfs;
  real rate_bg;
  vector[t_max] S_bg;
  vector[t_max] S0;
  vector[t_max] S_pred;

  # intercepts
  rate_os = exp(beta_os[1]);
  rate_pfs = exp(beta_pfs[1]);
  rate_bg = exp(beta_bg[1]);

  for (i in 1:t_max) {
    S_bg[i] = exp_Surv(i, rate_bg);
    S_os[i] = exp_Surv(i, rate_bg + rate_os);
    S_pfs[i] = exp_Surv(i, rate_bg + rate_pfs);

    S_os_pred[i] = curefrac*S_bg[i] + (1 - curefrac)*S_os[i];
    S_pfs_pred[i] = curefrac*S_bg[i] + (1 - curefrac)*S_pfs[i];
  }
}

