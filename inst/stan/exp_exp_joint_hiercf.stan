// exponential mixture cure model
// joint relative survival
// hierarchical cure fraction


functions {
#include /include/distributions.stan
}

// input data ----
data {
  int<lower=0> n_os;             // number of observations
  int<lower=0> n_pfs;
  int<lower=0> H_os;             // number of covariates
  int<lower=0> H_pfs;

  // vector<lower=t_pfs> [n_os] t_os;
  vector[n_os] t_os;             // observation times
  vector[n_pfs] t_pfs;

  vector[n_os] d_os;             // censoring indicator (1 = observed, 0 = censored)
  vector[n_pfs] d_pfs;

  matrix[n_os, H_os] X_os;       // matrix of covariates (with n rows and H columns)
  matrix[n_pfs, H_pfs] X_pfs;

  vector[H_os] mu_0_os;
  vector[H_pfs] mu_0_pfs;
  vector<lower=0> [H_os] sigma_0_os;
  vector<lower=0> [H_pfs] sigma_0_pfs;

  //TODO: what to do when different type/number covariates for os and pfs?
  vector[H_os] mu_bg;
  vector<lower=0> [H_os] sigma_bg;

  real mu_joint;
  real<lower=0> sigma_joint;

  real mean_cf;                  // cure fraction
  real sd_cf;
  real sd_cf_os;
  real sd_cf_pfs;

  int<lower=0> t_max;
}

parameters {
  vector[H_os] beta_os;       // coefficients in linear predictor (including intercept)
  vector[H_pfs] beta_pfs;
  vector[H_os] beta_bg;
  real beta_joint;

  real lp_cf_global;
  real lp_cf_os;
  real lp_cf_pfs;
}

transformed parameters {
  vector[n_os] lp_os;
  vector[n_pfs] lp_pfs;
  vector[n_os] lp_os_bg;
  vector[n_os] lp_pfs_bg;

  vector[n_os] lambda_os;
  vector[n_pfs] lambda_pfs;
  vector[n_os] lambda_os_bg;
  vector[n_os] lambda_pfs_bg;

  real<lower=0, upper=1> cf_global;
  real<lower=0, upper=1> cf_os;
  real<lower=0, upper=1> cf_pfs;

  # correlated event times
  lp_os = X_os*beta_os + beta_joint*(t_pfs - 1/exp(beta_pfs[1]));

  lp_pfs = X_pfs*beta_pfs;

  lp_os_bg = X_os*beta_bg;
  lp_pfs_bg = X_pfs*beta_bg;

  // rate parameters
  lambda_os = exp(lp_os);
  lambda_pfs = exp(lp_pfs);
  lambda_os_bg = exp(lp_os_bg);     // background survival with uncertainty
  lambda_pfs_bg = exp(lp_pfs_bg);

  cf_global = inv_logit(lp_cf_global);
  cf_os = inv_logit(lp_cf_os);
  cf_pfs = inv_logit(lp_cf_pfs);
}

model {
  beta_os ~ normal(mu_0_os, sigma_0_os);
  beta_pfs ~ normal(mu_0_pfs, sigma_0_pfs);
  beta_bg ~ normal(mu_bg, sigma_bg);
  beta_joint ~ normal(mu_joint, sigma_joint);

  lp_cf_global ~ normal(mean_cf, sd_cf);
  lp_cf_os ~ normal(lp_cf_global, sd_cf_os);
  lp_cf_pfs ~ normal(lp_cf_global, sd_cf_pfs);

  for (i in 1:n_os) {
    target += log_sum_exp(
                log(cf_os) +
                surv_exp_lpdf(t_os[i] | d_os[i], lambda_os_bg[i]),
                log1m(cf_os) +
                surv_exp_lpdf(t_os[i] | d_os[i], lambda_os_bg[i] + lambda_os[i])) +
              log_sum_exp(
                log(cf_pfs) +
                surv_exp_lpdf(t_pfs[i] | d_pfs[i], lambda_pfs_bg[i]),
                log1m(cf_pfs) +
                surv_exp_lpdf(t_pfs[i] | d_pfs[i], lambda_pfs_bg[i] + lambda_pfs[i]));
  }
}

generated quantities {
  # posterior
  real mean_os;
  real mean_pfs;
  real mean_bg;
  vector[t_max] S_bg;
  vector[t_max] S_os;
  vector[t_max] S_pfs;
  vector[t_max] S_os_pred;
  vector[t_max] S_pfs_pred;

  real pmean_os;
  real pmean_pfs;
  real pmean_bg;
  real pmean_cf;

  vector[t_max] pS_bg;
  vector[t_max] pS_os;
  vector[t_max] pS_pfs;
  vector[t_max] S_os_prior;
  vector[t_max] S_pfs_prior;

  real pbeta_os = normal_rng(mu_0_os[1], sigma_0_os[1]);
  real pbeta_pfs = normal_rng(mu_0_pfs[1], sigma_0_pfs[1]);
  real pbeta_bg = normal_rng(mu_bg[1], sigma_bg[1]);
  real pcurefrac = normal_rng(mean_cf, sd_cf);

  # intercepts
  mean_os = exp(beta_os[1]);
  mean_pfs = exp(beta_pfs[1]);
  mean_bg = exp(beta_bg[1]);

  for (i in 1:t_max) {
    S_bg[i] = exp_Surv(i, mean_bg);
    S_os[i] = exp_Surv(i, mean_bg + mean_os);
    S_pfs[i] = exp_Surv(i, mean_bg + mean_pfs);

    S_os_pred[i] = cf_os*S_bg[i] + (1 - cf_os)*S_os[i];
    S_pfs_pred[i] = cf_pfs*S_bg[i] + (1 - cf_pfs)*S_pfs[i];
  }

  # prior checks
  pmean_os = exp(pbeta_os);
  pmean_pfs = exp(pbeta_pfs);
  pmean_bg = exp(pbeta_bg);
  pmean_cf = inv_logit(pcurefrac);

  for (i in 1:t_max) {
    pS_bg[i] = exp_Surv(i, pmean_bg);
    pS_os[i] = exp_Surv(i, pmean_bg + pmean_os);
    pS_pfs[i] = exp_Surv(i, pmean_bg + pmean_pfs);

    S_os_prior[i] = pmean_cf*pS_bg[i] + (1 - pmean_cf)*pS_os[i];
    S_pfs_prior[i] = pmean_cf*pS_bg[i] + (1 - pmean_cf)*pS_pfs[i];
  }
}

