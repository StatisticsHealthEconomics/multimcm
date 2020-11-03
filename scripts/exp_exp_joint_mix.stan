// exponential mixture cure model
// joint relative survival

functions {
#include /include/distributions.stan
}

// input data ----
data {
  int<lower=0> n_os;             // number of observations
  int<lower=0> n_pfs;
  int<lower = 0> H_os;           // number of covariates
  int<lower = 0> H_pfs;
  vector[n_os] t_os;             // observed times
  vector[n_pfs] t_pfs;
  vector[n_os] d_os;             // censoring indicator (1 = observed, 0 = censored)
  vector[n_pfs] d_pfs;
  matrix[n_os, H_os] X_os;       // matrix of covariates (with n rows and H columns)
  matrix[n_pfs, H_pfs] X_pfs;

  vector[H_os] mu_os;
  vector[H_pfs] mu_pfs;
  vector<lower=0> [H_os] sigma_os;
  vector<lower=0> [H_pfs] sigma_pfs;

  //TODO: what to do when different type/number covariates for os and pfs?
  vector[H_os] mu_bg;
  vector<lower=0> [H_os] sigma_bg;

  real mu_joint;
  real<lower=0> sigma_joint;

  real a_cf;                  // cure fraction ~ Beta(a,b)
  real b_cf;

  int<lower=0> t_max;
}

parameters {
  vector[H_os] beta_os;       // coefficients in linear predictor (including intercept)
  vector[H_pfs] beta_pfs;
  vector[H_os] beta_bg;

  real<lower=0, upper=1> curefrac;
}

transformed parameters {
  vector[n_os] lp_os;
  vector[n_pfs] lp_pfs;
  vector[n_os] lp_bg;

  vector[n_os] lambda_os;
  vector[n_pfs] lambda_pfs;
  vector[n_os] lambda_bg;

  lp_os = X_os*beta_os + beta_joint*(t_pfs - mean(lambda_pfs));
  lp_pfs = X_pfs*beta_pfs;
  lp_os_bg = X_os*beta_bg;
  lp_pfs_bg = X_pfs*beta_bg;

  // rate parameters
  lambda_os = exp(lp_os);
  lambda_pfs = exp(lp_pfs);
  lambda_os_bg = exp(lp_os_bg);     // background survival with uncertainty
  lambda_pfs_bg = exp(lp_pfs_bg);
}

model {
  beta_os ~ normal(mu_os, sigma_os);
  beta_pfs ~ normal(mu_pfs, sigma_pfs);
  beta_bg ~ normal(mu_bg, sigma_bg);
  beta_joint ~ normal(mu_joint, sigma_joint);

  curefrac ~ beta(a_cf, b_cf);

  for (i in 1:n_os) {
    target += log_sum_exp(log(curefrac) +
                surv_exp_lpdf(t_os[i] | d_os[i], lambda_bg[i]),
                log1m(curefrac) +
                surv_exp_lpdf(t_os[i] | d_os[i], lambda_bg[i] + lambda_os[i])) +
              log_sum_exp(log(curefrac) +
                surv_exp_lpdf(t_pfs[i] | d_pfs[i], lambda_bg[i]),
                log1m(curefrac) +
                surv_exp_lpdf(t_pfs[i] | d_pfs[i], lambda_bg[i] + lambda_pfs[i]));
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

