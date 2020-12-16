// mixture cure model
// joint relative survival
//


functions {
#include /include/distributions.stan

  // // exponential integral used in gompertz mean
  // real exp_integral(real x, real xc, real[] theta,
  //                   real[] x_r, int[] x_i) {
  //   return exp(x)/x;
  // }
}

data {
  int<lower=0> n_os;            // number of observations
  int<lower=0> n_pfs;

  int<lower=0> H_os;            // number of covariates
  int<lower=0> H_pfs;

  vector[n_os] t_os;             // observation times
  vector[n_pfs] t_pfs;

  vector[n_os] d_os;             // censoring indicator (1 = observed, 0 = censored)
  vector[n_pfs] d_pfs;

  matrix[n_os, H_os] X_os;        // matrix of covariates (with n rows and H columns)
  matrix[n_pfs, H_pfs] X_pfs;

  int distn_os;                // 1: exp; 2: weibull; 3: gompertz
  int distn_pfs;

  real<lower=0> a_alpha_os[distn_os == 2 ? 1 : 0];
  real<lower=0> b_alpha_os[distn_os == 2 ? 1 : 0];

  real<lower=0> a_alpha_pfs[distn_pfs == 2 ? 1 : 0];
  real<lower=0> b_alpha_pfs[distn_pfs == 2 ? 1 : 0];

  vector[H_os] mu_0_os;
  vector[H_pfs] mu_0_pfs;
  vector<lower=0> [H_os] sigma_0_os;
  vector<lower=0> [H_pfs] sigma_0_pfs;

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
  real beta_joint;
  real<lower=0> alpha1[distn_os == 2 ? 1 : 0];
  real<lower=0> alpha2[distn_pfs == 2 ? 1 : 0];

  real<lower=0, upper=1> curefrac;
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
  vector[n_pfs] mean_t_pfs;

  // gompertz
  real x_r[0];
  int x_i[0];
  vector Ei[distn_pfs == 3 ? 1 : 0];

  lp_pfs = X_pfs*beta_pfs;
  lp_os_bg = X_os*beta_bg;
  lp_pfs_bg = X_pfs*beta_bg;

  // rate parameters
  lambda_pfs = exp(lp_pfs);
  lambda_os_bg = exp(lp_os_bg);
  lambda_pfs_bg = exp(lp_pfs_bg);

  // correlated event times ----

  // direct estimate
  // mean_t_pfs = mean(t_pfs)

  //TODO: impute censored t_pfs for conditional regression
  //TODO: \sum S_pfs(t) use for mean_t_pfs


  if (distn_pfs == 1) {
    // mean_t_pfs = 1/exp(beta_pfs[1]));  // global mean //TODO: should this be adjusted?
    for (i in 1:n_pfs)
      mean_t_pfs[i] = 1/lambda_pfs[i];
  }

  // // weibull
  // if (distn_pfs == 2) {
  //   // mean_t_pfs = exp(beta_pfs[1])*tgamma(1 + 1/alpha2);
  //   for (i in 1:n_pfs)
  //     mean_t_pfs[i] = lambda_pfs[i]*tgamma(1 + 1/alpha2);
  // }

  // // gompertz
  // if (distn_pfs == 3) {
  //   for (i in 1:n_pfs) {
  //     Ei[i] = integrate_1d (exp_integral, -lambda_pfs[i],
  //                           positive_infinity(),
  //                           {}, real[] x_r, int[] x_i)
  //     mean_t_pfs[i] = 1/b * exp(lambda_pfs[i]) * Ei[i]
  //   }
  // }

  lp_os = X_os*beta_os + beta_joint*(t_pfs - mean_t_pfs);
  lambda_os = exp(lp_os);
}

model {
  vector[n_os] distn_os_lpdf;
  vector[n_pfs] distn_pfs_lpdf;

  beta_os ~ normal(mu_0_os, sigma_0_os);
  beta_pfs ~ normal(mu_0_pfs, sigma_0_pfs);
  beta_bg ~ normal(mu_bg, sigma_bg);
  beta_joint ~ normal(mu_joint, sigma_joint);

  // weibull
  if (distn_os == 2)
    alpha1 ~ gamma(a_alpha_os, b_alpha_os);
  if (distn_pfs == 2)
    alpha2 ~ gamma(a_alpha_pfs, b_alpha_pfs);

  // gompertz
  if (distn_os == 3)
    alpha1 ~ gamma(a_alpha_os, b_alpha_os);
  if (distn_pfs == 3)
    alpha2 ~ gamma(a_alpha_pfs, b_alpha_pfs);

  curefrac ~ beta(a_cf, b_cf);

  for (i in 1:n_os) {

    if (distn_os == 1)
      distn_os_lpdf[i] = surv_exp_lpdf(t_os[i] | d_os[i], lambda_os_bg[i] + lambda_os[i]);
    // if (distn_os == 2)
    //   distn_os_lpdf[i] = joint_exp_weibull_lpdf(t_os[i] | d_os[i], alpha1, lambda_os[i], lambda_os_bg[i]);
    // if (distn_os == 3)
    //   distn_os_lpdf[i] = joint_exp_gompertz_lpdf(t_os[i] | d_os[i], alpha1, lambda_os[i], lambda_os_bg[i]);

    if (distn_pfs == 1)
      distn_pfs_lpdf[i] = surv_exp_lpdf(t_pfs[i] | d_pfs[i], lambda_pfs_bg[i] + lambda_pfs[i]);
    // if (distn_pfs == 2)
    //   distn_pfs_lpdf[i] = joint_exp_weibull_lpdf(t_pfs[i] | d_pfs[i], alpha2, lambda_pfs[i], lambda_pfs_bg[i]);
    // if (distn_pfs == 3)
    //   distn_pfs_lpdf[i] = joint_exp_gompertz_lpdf(t_pfs[i] | d_pfs[i], alpha2, lambda_pfs[i], lambda_pfs_bg[i]);

    target += log_sum_exp(
                log(curefrac) + surv_exp_lpdf(t_os[i] | d_os[i], lambda_os_bg[i]),
                log1m(curefrac) + distn_os_lpdf[i]) +
              log_sum_exp(
                log(curefrac) + surv_exp_lpdf(t_pfs[i] | d_pfs[i], lambda_pfs_bg[i]),
                log1m(curefrac) + distn_pfs_lpdf[i]);
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
  // real pcurefrac;

  real palpha1[distn_os == 2 ? 1 : 0];
  real palpha2[distn_pfs == 2 ? 1 : 0];

  vector[t_max] pS_bg;
  vector[t_max] pS_os;
  vector[t_max] pS_pfs;
  vector[t_max] S_os_prior;
  vector[t_max] S_pfs_prior;

  real pbeta_os = normal_rng(mu_0_os[1], sigma_0_os[1]);
  real pbeta_pfs = normal_rng(mu_0_pfs[1], sigma_0_pfs[1]);
  real pbeta_bg = normal_rng(mu_bg[1], sigma_bg[1]);
  real pcurefrac = beta_rng(a_cf, b_cf);

  // weibull
  if (distn_os == 2)
    palpha1 = gamma_rng(a_alpha_os, b_alpha_os);
  if (distn_pfs == 2)
    palpha2 = gamma_rng(a_alpha_pfs, b_alpha_pfs);

  # intercepts
  mean_os = exp(beta_os[1]);
  mean_pfs = exp(beta_pfs[1]);
  mean_bg = exp(beta_bg[1]);

  for (i in 1:t_max) {
    S_bg[i] = exp_Surv(i, mean_bg);

  if (distn_os == 1)
    S_os[i] = exp_Surv(i, mean_os);
  // if (distn_os == 2)
  //   S_os[i] = weibull_Surv(i, alpha1, mean_os);

  if (distn_pfs == 1)
    S_pfs[i] = exp_Surv(i, mean_pfs);
  // if (distn_pfs == 2)
  //   S_pfs[i] = weibull_Surv(i, alpha2, mean_pfs);

    S_os_pred[i] = curefrac*S_bg[i] + (1 - curefrac)*S_os[i]*S_bg[i];
    S_pfs_pred[i] = curefrac*S_bg[i] + (1 - curefrac)*S_pfs[i]*S_bg[i];
  }

  # prior checks
  pmean_os = exp(pbeta_os);
  pmean_pfs = exp(pbeta_pfs);
  pmean_bg = exp(pbeta_bg);

  // // point estimation
  // pmean_os = exp(mu_0_os[1]);
  // pmean_pfs = exp(mu_0_pfs[1]);
  // pmean_bg = exp(mu_bg[1]);
  // pcurefrac = a_cf/(a_cf + b_cf);

  for (i in 1:t_max) {
    pS_bg[i] = exp_Surv(i, pmean_bg);

    if (distn_os == 1)
      pS_os[i] = exp_Surv(i, pmean_os);
    // if (distn_os == 2)
    //   pS_os[i] = weibull_Surv(i, palpha1, pmean_os);

    if (distn_pfs == 1)
      pS_pfs[i] = exp_Surv(i, pmean_pfs);
    // if (distn_pfs == 2)
    //   pS_pfs[i] = weibull_Surv(i, palpha2, pmean_pfs);

    S_os_prior[i] = pcurefrac*pS_bg[i] + (1 - pcurefrac)*pS_os[i]*pS_bg[i];
    S_pfs_prior[i] = pcurefrac*pS_bg[i] + (1 - pcurefrac)*pS_pfs[i]*pS_bg[i];
  }
}

